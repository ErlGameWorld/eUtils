#include <erl_nif.h>
#include <chrono>
#include <limits>
#include <atomic>
#include <algorithm> // for std::max, std::min

#if __cplusplus < 202002L
#error "Current compiler does not support C++20. Please use GCC 13+, Clang 15+, or MSVC 19.29+"
#endif

using namespace std::chrono;

// ============================================================================
// Globals & Constants
// ============================================================================

namespace {
    // Atoms
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;
    ERL_NIF_TERM atomUndefined;
    ERL_NIF_TERM atomEarliest;
    ERL_NIF_TERM atomLatest;
    ERL_NIF_TERM atomStrict;
    ERL_NIF_TERM atomError;

    // Error Atoms
    ERL_NIF_TERM atomErrZoneNotLoaded;
    ERL_NIF_TERM atomErrInternal;
    ERL_NIF_TERM atomErrInvalidDateTime;
    ERL_NIF_TERM atomErrAmbiguous;
    ERL_NIF_TERM atomErrNonexistent;
    ERL_NIF_TERM atomErrInvalidStrategy;
    ERL_NIF_TERM atomErrInvalidDst;

    // Global Timezone Pointer
    static std::atomic<const time_zone *> g_zone{nullptr};

    // ============================================================================
    // Thread Local Cache
    // ============================================================================

    struct UtcCache {
        ErlNifSInt64 offset{0};
        ErlNifSInt64 valid_from{std::numeric_limits<ErlNifSInt64>::max()};
        ErlNifSInt64 valid_until{std::numeric_limits<ErlNifSInt64>::min()};
        bool is_dst{false};
        bool initialized{false};
    };

    struct LocalCache {
        ErlNifSInt64 offset{0};
        ErlNifSInt64 valid_from{std::numeric_limits<ErlNifSInt64>::max()};
        ErlNifSInt64 valid_until{std::numeric_limits<ErlNifSInt64>::min()};
        bool initialized{false};
    };

    struct ThreadCache {
        UtcCache utc;
        LocalCache local;
    };

    thread_local ThreadCache t_cache;

    // ============================================================================
    // Helpers
    // ============================================================================

    static inline ERL_NIF_TERM make_error(ErlNifEnv *env, ERL_NIF_TERM reason) {
        return enif_make_tuple2(env, atomError, reason);
    }

    static inline bool add_overflow_64(ErlNifSInt64 a, ErlNifSInt64 b, ErlNifSInt64 *out) {
        if (b > 0 && a > std::numeric_limits<ErlNifSInt64>::max() - b) return true;
        if (b < 0 && a < std::numeric_limits<ErlNifSInt64>::min() - b) return true;
        *out = a + b;
        return false;
    }

    // ----------------------------------------------------------------------------
    // Cache Logic
    // ----------------------------------------------------------------------------

    static void update_utc_cache(ThreadCache &cache, const sys_info &info) {
        cache.utc.offset = duration_cast<seconds>(info.offset).count();
        cache.utc.is_dst = (info.save != seconds{0});

        if (info.begin == sys_seconds::min())
            cache.utc.valid_from = std::numeric_limits<ErlNifSInt64>::min();
        else
            cache.utc.valid_from = duration_cast<seconds>(info.begin.time_since_epoch()).count();

        if (info.end == sys_seconds::max())
            cache.utc.valid_until = std::numeric_limits<ErlNifSInt64>::max();
        else
            cache.utc.valid_until = duration_cast<seconds>(info.end.time_since_epoch()).count();

        cache.utc.initialized = true;
    }

    static void update_local_cache(ThreadCache &cache, const time_zone *z, const local_info &info) {
        if (info.result != local_info::unique) {
            cache.local.initialized = false;
            return;
        }

        const sys_info &si = info.first;
        const ErlNifSInt64 current_offset = duration_cast<seconds>(si.offset).count();

        ErlNifSInt64 prev_offset = current_offset;
        ErlNifSInt64 next_offset = current_offset;

        try {
            if (si.begin != sys_seconds::min()) {
                sys_info prev = z->get_info(si.begin - seconds{1});
                prev_offset = duration_cast<seconds>(prev.offset).count();
            }
            if (si.end != sys_seconds::max()) {
                sys_info next = z->get_info(si.end);
                next_offset = duration_cast<seconds>(next.offset).count();
            }
        } catch (...) {
            cache.local.initialized = false;
            return;
        }

        ErlNifSInt64 utc_begin_sec = (si.begin == sys_seconds::min())
            ? std::numeric_limits<ErlNifSInt64>::min()
            : duration_cast<seconds>(si.begin.time_since_epoch()).count();

        ErlNifSInt64 utc_end_sec = (si.end == sys_seconds::max())
            ? std::numeric_limits<ErlNifSInt64>::max()
            : duration_cast<seconds>(si.end.time_since_epoch()).count();

        ErlNifSInt64 local_from;
        if (utc_begin_sec == std::numeric_limits<ErlNifSInt64>::min()) {
            local_from = std::numeric_limits<ErlNifSInt64>::min();
        } else {
            ErlNifSInt64 effective_offset = std::max(current_offset, prev_offset);
            if (add_overflow_64(utc_begin_sec, effective_offset, &local_from)) {
                cache.local.initialized = false; return;
            }
        }

        ErlNifSInt64 local_until;
        if (utc_end_sec == std::numeric_limits<ErlNifSInt64>::max()) {
            local_until = std::numeric_limits<ErlNifSInt64>::max();
        } else {
            ErlNifSInt64 effective_offset = std::min(current_offset, next_offset);
            if (add_overflow_64(utc_end_sec, effective_offset, &local_until)) {
                cache.local.initialized = false; return;
            }
        }

        if (local_from >= local_until) {
            cache.local.initialized = false;
            return;
        }

        cache.local.offset = current_offset;
        cache.local.valid_from = local_from;
        cache.local.valid_until = local_until;
        cache.local.initialized = true;
    }

    // ----------------------------------------------------------------------------
    // Parsing & Formatting
    // ----------------------------------------------------------------------------

    static bool parse_datetime(ErlNifEnv *env, ERL_NIF_TERM dt, int &Y, int &M, int &D, int &h, int &m, int &s, int &us) {
        const ERL_NIF_TERM *tuple;
        int arity;
        if (!enif_get_tuple(env, dt, &arity, &tuple) || arity != 2) return false;

        const ERL_NIF_TERM *date;
        int date_arity;
        if (!enif_get_tuple(env, tuple[0], &date_arity, &date) || date_arity != 3) return false;

        const ERL_NIF_TERM *time;
        int time_arity;
        if (!enif_get_tuple(env, tuple[1], &time_arity, &time) || (time_arity != 3 && time_arity != 4)) return false;

        if (!enif_get_int(env, date[0], &Y) ||
            !enif_get_int(env, date[1], &M) ||
            !enif_get_int(env, date[2], &D)) return false;

        if (!enif_get_int(env, time[0], &h) ||
            !enif_get_int(env, time[1], &m) ||
            !enif_get_int(env, time[2], &s)) return false;

        us = 0;
        if (time_arity == 4 && !enif_get_int(env, time[3], &us)) return false;

        // [New Fix] Year Range Check to prevent UB in sys_days calculation
        // std::chrono::year range is implementation defined but at least covers [-32767, 32767]
        if (Y < int(year::min()) || Y > int(year::max())) return false;

        if (M < 1 || M > 12 || D < 1 || D > 31) return false;
        if (h < 0 || h > 23 || m < 0 || m > 59 || s < 0 || s > 59 || us < 0 || us > 999999) return false;

        year_month_day ymd{year{Y}, month{(unsigned) M}, day{(unsigned) D}};
        // Use sys_days conversion to check if date is valid (e.g. leap years, correct days in month)
        // ymd.ok() checks fields range, converting to sys_days and back or relying on ok() is enough for strictness.
        // Here we rely on ymd.ok() as standard check.
        return ymd.ok();
    }

    template<typename TimePoint>
    static ERL_NIF_TERM make_datetime_tuple(ErlNifEnv *env, const TimePoint &tp) {
        auto day_tp = floor<days>(tp);
        year_month_day ymd{sys_days{day_tp.time_since_epoch()}};
        hh_mm_ss<microseconds> hms{tp - day_tp};

        ERL_NIF_TERM date_term = enif_make_tuple3(env,
            enif_make_int64(env, (ErlNifSInt64)int(ymd.year())),
            enif_make_int(env, int(unsigned(ymd.month()))),
            enif_make_int(env, int(unsigned(ymd.day())))
        );
        ERL_NIF_TERM time_term = enif_make_tuple3(env,
            enif_make_int64(env, hms.hours().count()),
            enif_make_int64(env, hms.minutes().count()),
            enif_make_int64(env, hms.seconds().count())
        );

        return enif_make_tuple2(env, date_term, time_term);
    }

    // ============================================================================
    // API Implementation
    // ============================================================================

    static ERL_NIF_TERM nif_zone_offset(ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM /*argv*/[]) {
        const time_zone *z = g_zone.load(std::memory_order_acquire);
        if (!z) return make_error(env, atomErrZoneNotLoaded);

        ThreadCache &cache = t_cache;
        const auto now_floor = floor<seconds>(system_clock::now());
        const ErlNifSInt64 now_sec = now_floor.time_since_epoch().count();

        try {
            if (!cache.utc.initialized || now_sec < cache.utc.valid_from || now_sec >= cache.utc.valid_until) {
                const sys_info info = z->get_info(now_floor);
                update_utc_cache(cache, info);
            }
            return enif_make_int64(env, cache.utc.offset);
        } catch (...) {
            return make_error(env, atomErrInternal);
        }
    }

    static ERL_NIF_TERM nif_universal_time_to_local_time(ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[]) {
        const time_zone *z = g_zone.load(std::memory_order_acquire);
        if (!z) return make_error(env, atomErrZoneNotLoaded);

        int Y, M, D, h, m, s, us;
        if (!parse_datetime(env, argv[0], Y, M, D, h, m, s, us)) return make_error(env, atomErrInvalidDateTime);

        year_month_day ymd{year{Y}, month{(unsigned) M}, day{(unsigned) D}};
        sys_days sd{ymd};
        sys_time<microseconds> utc_tp = sys_time<microseconds>{sd.time_since_epoch()} + hours{h} + minutes{m} + seconds{s} + microseconds{us};

        const auto utc_sec_floor = floor<seconds>(utc_tp);
        const ErlNifSInt64 utc_sec = utc_sec_floor.time_since_epoch().count();

        ThreadCache &cache = t_cache;

        try {
            if (!cache.utc.initialized || utc_sec < cache.utc.valid_from || utc_sec >= cache.utc.valid_until) {
                const sys_info info = z->get_info(utc_sec_floor);
                update_utc_cache(cache, info);
            }

            const auto local_tp = utc_tp + seconds{cache.utc.offset};
            return make_datetime_tuple(env, local_tp);

        } catch (...) {
            return make_error(env, atomErrInternal);
        }
    }

    static ERL_NIF_TERM local_to_universal_impl(ErlNifEnv *env, const time_zone *z, ERL_NIF_TERM dt_term, int strategy, int dst_input, bool return_list) {
        int Y, M, D, h, m, s, us;
        if (!parse_datetime(env, dt_term, Y, M, D, h, m, s, us)) return make_error(env, atomErrInvalidDateTime);

        year_month_day ymd{year{Y}, month{(unsigned) M}, day{(unsigned) D}};
        local_days ld{sys_days{ymd}.time_since_epoch()};
        local_time<microseconds> local_tp = local_time<microseconds>{ld.time_since_epoch()}
                                            + hours{h} + minutes{m} + seconds{s} + microseconds{us};

        ThreadCache &cache = t_cache;
        const auto local_sec_floor = floor<seconds>(local_tp);
        const ErlNifSInt64 local_sec = local_sec_floor.time_since_epoch().count();

        // ------------------------------------------------
        // Fast Path (Cache Hit)
        // ------------------------------------------------
        if (cache.local.initialized && local_sec >= cache.local.valid_from && local_sec < cache.local.valid_until) {
            sys_time<microseconds> utc_tp = sys_time<microseconds>{local_tp.time_since_epoch() - seconds{cache.local.offset}};
            ERL_NIF_TERM tuple = make_datetime_tuple(env, utc_tp);
            return return_list ? enif_make_list1(env, tuple) : tuple;
        }

        // ------------------------------------------------
        // Slow Path
        // ------------------------------------------------
        try {
            local_info li = z->get_info(local_tp);

            // Update Cache
            update_local_cache(cache, z, li);

            // Case 1: Unique
            if (li.result == local_info::unique) {
                auto offset = duration_cast<seconds>(li.first.offset);
                sys_time<microseconds> result = sys_time<microseconds>{local_tp.time_since_epoch() - offset};
                ERL_NIF_TERM tuple = make_datetime_tuple(env, result);
                return return_list ? enif_make_list1(env, tuple) : tuple;
            }

            // Case 2: Ambiguous
            if (li.result == local_info::ambiguous) {
                if (return_list && strategy == 0) {
                    return enif_make_list2(env,
                        make_datetime_tuple(env, z->to_sys(local_tp, choose::earliest)),
                        make_datetime_tuple(env, z->to_sys(local_tp, choose::latest)));
                }

                if (strategy == 3) return make_error(env, atomErrAmbiguous);

                choose c = choose::earliest;
                if (dst_input == 1) c = choose::earliest;
                else if (dst_input == 0) c = choose::latest;
                else if (strategy == 2) c = choose::latest;

                sys_time<microseconds> result = z->to_sys(local_tp, c);
                ERL_NIF_TERM tuple = make_datetime_tuple(env, result);
                return return_list ? enif_make_list1(env, tuple) : tuple;
            }

            // Case 3: Nonexistent
            if (li.result == local_info::nonexistent) {
                if (strategy == 3) return make_error(env, atomErrNonexistent);
                sys_time<microseconds> result = time_point_cast<microseconds>(li.second.begin);
                ERL_NIF_TERM tuple = make_datetime_tuple(env, result);
                return return_list ? enif_make_list1(env, tuple) : tuple;
            }

            return make_error(env, atomErrInternal);
        } catch (...) {
            return make_error(env, atomErrInternal);
        }
    }

    static ERL_NIF_TERM nif_local_time_to_universal_time(ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[]) {
        const time_zone *z = g_zone.load(std::memory_order_acquire);
        if (!z) return make_error(env, atomErrZoneNotLoaded);
        // return_list = false -> Returns Tuple
        return local_to_universal_impl(env, z, argv[0], 0, -1, false);
    }

    static ERL_NIF_TERM nif_local_time_to_universal_time3(ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[]) {
        const time_zone *z = g_zone.load(std::memory_order_acquire);
        if (!z) return make_error(env, atomErrZoneNotLoaded);

        // Strategy Parsing
        int strategy = -1;
        if (enif_is_atom(env, argv[1])) {
            if (enif_is_identical(argv[1], atomUndefined)) strategy = 0;
            else if (enif_is_identical(argv[1], atomEarliest)) strategy = 1;
            else if (enif_is_identical(argv[1], atomLatest)) strategy = 2;
            else if (enif_is_identical(argv[1], atomStrict)) strategy = 3;
        } else {
            enif_get_int(env, argv[1], &strategy);
        }

        if (strategy < 0 || strategy > 3) return make_error(env, atomErrInvalidStrategy);

        // DST Input Parsing
        int dst_input = -2;
        if (enif_is_atom(env, argv[2])) {
            if (enif_is_identical(argv[2], atomUndefined)) dst_input = -1;
            else if (enif_is_identical(argv[2], atomTrue)) dst_input = 1;
            else if (enif_is_identical(argv[2], atomFalse)) dst_input = 0;
        } else {
            int val;
            if (enif_get_int(env, argv[2], &val)) {
                if (val == 0 || val == 1) dst_input = val;
                else return make_error(env, atomErrInvalidDst);
            } else {
                return make_error(env, atomErrInvalidDst);
            }
        }

        if (dst_input == -2) return make_error(env, atomErrInvalidDst);

        if (strategy == 1) dst_input = 1;
        if (strategy == 2) dst_input = 0;

        // return_list = true -> Returns List
        return local_to_universal_impl(env, z, argv[0], strategy, dst_input, true);
    }

    // ============================================================================
    // Init
    // ============================================================================

    static int nif_load(ErlNifEnv *env, void ** /*priv_data*/, ERL_NIF_TERM /*load_info*/) {
        atomTrue = enif_make_atom(env, "true");
        atomFalse = enif_make_atom(env, "false");
        atomUndefined = enif_make_atom(env, "undefined");
        atomEarliest = enif_make_atom(env, "earliest");
        atomLatest = enif_make_atom(env, "latest");
        atomStrict = enif_make_atom(env, "strict");
        atomError = enif_make_atom(env, "error");

        atomErrZoneNotLoaded = enif_make_atom(env, "zone_not_loaded");
        atomErrInternal = enif_make_atom(env, "internal_error");
        atomErrInvalidDateTime = enif_make_atom(env, "invalid_date_time");
        atomErrAmbiguous = enif_make_atom(env, "ambiguous_time");
        atomErrNonexistent = enif_make_atom(env, "nonexistent_time");
        atomErrInvalidStrategy = enif_make_atom(env, "invalid_strategy");
        atomErrInvalidDst = enif_make_atom(env, "invalid_dst_input");

        try {
            const auto &db = get_tzdb();
            g_zone.store(db.current_zone(), std::memory_order_release);
        } catch (...) {
            g_zone.store(nullptr, std::memory_order_release);
        }
        return 0;
    }

    static ERL_NIF_TERM nif_is_available(ErlNifEnv */*env*/, int /*argc*/, const ERL_NIF_TERM /*argv*/[]) {
        return (g_zone.load(std::memory_order_acquire) != nullptr) ? atomTrue : atomFalse;
    }

    static ErlNifFunc nif_funcs[] = {
        {"is_available", 0, nif_is_available, 0},
        {"zone_offset", 0, nif_zone_offset, 0},
        {"universal_time_to_local_time", 1, nif_universal_time_to_local_time, 0},
        {"local_time_to_universal_time", 1, nif_local_time_to_universal_time, 0},
        {"local_time_to_universal_time", 3, nif_local_time_to_universal_time3, 0},
    };
} // namespace

extern "C" {
ERL_NIF_INIT(eNifFtz, nif_funcs, nif_load, NULL, NULL, NULL)
}