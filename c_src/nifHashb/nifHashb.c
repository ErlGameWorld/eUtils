#include <stdint.h>
#include <string.h>
#include "erl_nif.h"

typedef struct {
    size_t keySize;
    size_t bblSize_1;
    size_t bblSize_2;
    struct _bbl **arrayPtr;
} hashb;

typedef struct _bbl {
    struct _node *head;
} bbl;

typedef struct _node {
    struct _node *next;
    ErlNifBinary Key;
    ErlNifBinary Value;
} node;

#define BBLSIZE_1 128
#define BBLSIZE_2 128

static ErlNifResourceType *ResType = NULL;
static ERL_NIF_TERM undefined;
static ERL_NIF_TERM ok;

///////////////////////////////hash 函数 /////////////////////////////
static uint32_t fnv_hash(const unsigned char *data, size_t size, uint32_t salt) {
    uint32_t i;
    uint64_t hashv;
    const unsigned char *_hf_key = (const unsigned char *) (data);
    hashv = 2166136261;
    for (i = 0; i < size; i++) {
        hashv = hashv ^ _hf_key[i];
        hashv = hashv * 16777619;
    }
    //enif_fprintf(stdout, "IMY************get  %T \n", argv[1]);
    return hashv;
}

static uint32_t murmurhash(const unsigned char *key, uint32_t len, uint32_t seed) {
    //uint32_t c1 = 0xcc9e2d51;
    //uint32_t c2 = 0x1b873593;
    //uint32_t r1 = 15;
    //uint32_t r2 = 13;
    //uint32_t m = 5;
    //uint32_t n = 0xe6546b64;
    uint32_t h = 0;
    uint32_t k = 0;
    uint8_t *d = (uint8_t *) key;             // 32 bit extract from `key'
    const uint32_t *chunks = NULL;
    const uint8_t *tail = NULL;               // tail - last 8 bytes
    int i = 0;
    int l = len / 4;                          // chunk length

    h = seed;

    chunks = (const uint32_t *) (d + l * 4);  // body
    tail = (const uint8_t *) (d + l * 4);     // last 8 byte chunk of `key'

    // for each 4 byte chunk of `key'
    for (i = -l; i != 0; ++i) {
        // next 4 byte chunk of `key'
        k = chunks[i];

        // encode next 4 byte chunk of `key'
        k *= 0xcc9e2d51;
        k = (k << 15) | (k >> (32 - 15));
        k *= 0x1b873593;

        // append to hash
        h ^= k;
        h = (h << 13) | (h >> (32 - 13));
        h = h * 5 + 0xe6546b64;
    }

    k = 0;

    // remainder
    switch (len & 3) { // `len % 4'
        case 3:
            k ^= (tail[2] << 16);
        case 2:
            k ^= (tail[1] << 8);
        case 1:
            k ^= tail[0];
            k *= 0xcc9e2d51;
            k = (k << 15) | (k >> (32 - 15));
            k *= 0x1b873593;
            h ^= k;
    }

    h ^= len;

    h ^= (h >> 16);
    h *= 0x85ebca6b;
    h ^= (h >> 13);
    h *= 0xc2b2ae35;
    h ^= (h >> 16);

    return h;
}

//////////////////////////////////hash 函数测试///////////////////////////
static ERL_NIF_TERM hash1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary Value;
    if (!enif_term_to_binary(env, argv[0], &Value))
        return enif_make_badarg(env);

    uint32_t Salt;
    if (!enif_get_uint(env, argv[1], &Salt)) {
        return enif_make_badarg(env);
    }

    Salt = fnv_hash(Value.data, Value.size, Salt);
    enif_release_binary(&Value);
    return ok;

};

static ERL_NIF_TERM hash2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifUInt64 Salt;
    if (!enif_get_uint64(env, argv[1], &Salt))
        return enif_make_badarg(env);

    ErlNifBinary Value;
    if (!enif_term_to_binary(env, argv[0], &Value))
        return enif_make_badarg(env);
    enif_release_binary(&Value);
    Salt = enif_hash(ERL_NIF_INTERNAL_HASH, argv[1], 2423432432);
    return ok;
};

static ERL_NIF_TERM hash3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifUInt64 Salt;
    if (!enif_get_uint64(env, argv[1], &Salt))
        return enif_make_badarg(env);

    ErlNifBinary Value;
    if (!enif_term_to_binary(env, argv[0], &Value))
        return enif_make_badarg(env);
    Salt = murmurhash(Value.data, Value.size, 0);
    enif_release_binary(&Value);
    return ok;
};

static uint8_t compareChar1(const unsigned char *src, const unsigned char *dst, size_t Size) {

    // int j = 0;
    // unsigned char *tsrc, *tdst;
    // tsrc = src;
    // tdst = dst;
    // for(j = 0; j <= Size; j++)
    // {
    //     enif_fprintf(stdout, "IMY************get9999111 src %d dst %d \n", *tsrc, *tdst);
    //     tsrc++;
    //     tdst++;
    // }
    uint8_t i = 0;
    while (i < Size-1 && (*src == *dst)) {
       // enif_fprintf(stdout, "IMY************get99992222 %d %d \n", *src, *dst);
        src++;
        dst++;
        i++;
    }
    //enif_fprintf(stdout, "IMY************get9999while end %d %d \n", *src, *dst);
    if (*src != *dst) {
        //enif_fprintf(stdout, "IMY************get99993333 %d %d \n", *src, *dst);
        return 1;
    } else {
        //enif_fprintf(stdout, "IMY************get99995555 %d %d \n", *src, *dst);
        return 0;
    }
}

static uint8_t compareChar2(const unsigned char *src, const unsigned char *dst, size_t Size) {
    const uint32_t *intSrc = NULL;
    const uint32_t *intDst = NULL;
    const uint8_t *tailSrc = NULL;               // tail - last 8 bytes
    const uint8_t *tailDst = NULL;               // tail - last 8 bytes
    int l = Size / 4;                            // chunk length

    intSrc = (const uint32_t *) src;      // body
    intDst = (const uint32_t *) dst;      // body
    tailSrc = (const uint8_t *) (src + l * 4);     // last 8 byte chunk of `key'
    tailDst = (const uint8_t *) (dst + l * 4);     // last 8 byte chunk of `key'

    // for each 4 byte chunk of `key'
    int i = 0;
    for (i = 0; i < l; i++) {
        if (intSrc[i] != intDst[i])
            return 1;
    }
    // remainder
    switch (Size & 3) {
        case 3:
            if (tailSrc[0] != tailDst[0] || tailSrc[1] != tailDst[1] || tailSrc[2] != tailDst[2]) {
                return 1;
            }
            break;
        case 2:
            if (tailSrc[0] != tailDst[0] || tailSrc[1] != tailDst[1]) {
                return 1;
            }
            break;
        case 1:
            if (tailSrc[0] != tailDst[0]) {
                return 1;
            }
            break;
    }
    return 0;
}

static ERL_NIF_TERM compareBin1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary src, dst;

    if (!(enif_inspect_binary(env, argv[0], &src) && enif_inspect_binary(env, argv[1], &dst)))
        return enif_make_badarg(env);

    if (src.size != dst.size) {
        return enif_make_int(env, 1);
    }
    int Ret = compareChar1(src.data, dst.data, src.size);
    return enif_make_int(env, Ret);
}

static ERL_NIF_TERM compareBin2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary src, dst;
    if (!(enif_inspect_binary(env, argv[0], &src) && enif_inspect_binary(env, argv[1], &dst)))
        return enif_make_badarg(env);

    if (src.size != dst.size) {
        return enif_make_int(env, 1);
    }
    int Ret = compareChar2(src.data, dst.data, src.size);
    return enif_make_int(env, Ret);
}

/////////////////////////////////////////////////////////////////////

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info) {
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ResType = enif_open_resource_type(env, NULL, "_nifHashb_", NULL, flags, NULL);
    if (NULL == ResType)
        return -1;
    undefined = enif_make_atom(env, "undefined");
    ok = enif_make_atom(env, "ok");
    return 0;
}

static ERL_NIF_TERM new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    hashb *ResPtr = (hashb *) enif_alloc_resource(ResType, sizeof(hashb));
    ResPtr->keySize = 0;
    ResPtr->bblSize_1 = BBLSIZE_1;
    ResPtr->bblSize_2 = BBLSIZE_2;
    ResPtr->arrayPtr = enif_alloc(sizeof(bbl *) * BBLSIZE_1);
    int i, j;
    bbl NullBbl;
    NullBbl.head = NULL;
    for (i = 0; i < BBLSIZE_1; i++) {
        ResPtr->arrayPtr[i] = enif_alloc(sizeof(bbl) * BBLSIZE_2);
        for (j = 0; j < BBLSIZE_2; j++) {
            ResPtr->arrayPtr[i][j] = NullBbl;
        }
    }
    ERL_NIF_TERM Res = enif_make_resource(env, ResPtr);
    enif_release_resource(ResPtr);
    return Res;
}

static uint8_t compareChar(const unsigned char *src, const unsigned char *dst, size_t Size) {
    const uint32_t *intSrc = NULL;
    const uint32_t *intDst = NULL;
    const uint8_t *tailSrc = NULL;                  // tail - last 8 bytes
    const uint8_t *tailDst = NULL;                  // tail - last 8 bytes
    int l = Size / 4;                               // chunk length

    intSrc = (const uint32_t *) src;                // body
    intDst = (const uint32_t *) dst;                // body
    tailSrc = (const uint8_t *) (src + l * 4);      // last 8 byte chunk of `key'
    tailDst = (const uint8_t *) (dst + l * 4);      // last 8 byte chunk of `key'

    // for each 4 byte chunk of `key'
    int i = 0;
    for (i = 0; i < l; i++) {
        if (intSrc[i] != intDst[i])
            return 1;
    }
    // remainder
    switch (Size & 3) {
        case 3:
            if (tailSrc[0] != tailDst[0] || tailSrc[1] != tailDst[1] || tailSrc[2] != tailDst[2]) {
                return 1;
            }
            break;
        case 2:
            if (tailSrc[0] != tailDst[0] || tailSrc[1] != tailDst[1]) {
                return 1;
            }
            break;
        case 1:
            if (tailSrc[0] != tailDst[0]) {
                return 1;
            }
            break;
    }
    return 0;
}

static uint8_t compareBin(ErlNifBinary src, ErlNifBinary dst) {
    if (src.size != dst.size) {
        //enif_fprintf(stdout, "IMY************get8888\n");
        return 1;
    }
    //enif_fprintf(stdout, "IMY************get8888111 %d \n", src.size);
    return compareChar(src.data, dst.data, src.size);
}

static ERL_NIF_TERM get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    hashb *ResPtr;

    if (!enif_get_resource(env, argv[0], ResType, (void **) &ResPtr)) {
        return enif_make_badarg(env);
    }
    //enif_fprintf(stdout, "IMY************get1111\n");

    ErlNifBinary getKey;
    if (!enif_term_to_binary(env, argv[1], &getKey))
        return enif_make_badarg(env);

    //enif_fprintf(stdout, "IMY************get2222\n");
    uint32_t BblIndex_1, BblIndex_2;
    BblIndex_1 = murmurhash(getKey.data, getKey.size, 131) % ResPtr->bblSize_1;
    BblIndex_2 = murmurhash(getKey.data, getKey.size, 16777619) % ResPtr->bblSize_2;
    //enif_fprintf(stdout, "IMY************get3333\n");
    if (NULL == ResPtr->arrayPtr[BblIndex_1][BblIndex_2].head) {
        //enif_fprintf(stdout, "IMY************get4444\n");
        enif_release_binary(&getKey);
        return undefined;
    } else {
       // enif_fprintf(stdout, "IMY************get55555\n");
        node *Head = ResPtr->arrayPtr[BblIndex_1][BblIndex_2].head;
        while (Head) {
           // enif_fprintf(stdout, "IMY************get7777\n");
            if (compareBin(getKey, Head->Key) == 0) {
                ERL_NIF_TERM Ret;
                enif_binary_to_term(env, Head->Value.data, Head->Value.size, &Ret, 0);
                //enif_fprintf(stdout, "IMY************get5555\n");
                enif_release_binary(&getKey);
                return Ret;
            }
            Head = Head->next;
        }
        //enif_fprintf(stdout, "IMY************get6666\n");
        enif_release_binary(&getKey);
        return undefined;
    }
}

static ERL_NIF_TERM put(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    hashb *ResPtr;

    if (!enif_get_resource(env, argv[0], ResType, (void **) &ResPtr)) {
        return enif_make_badarg(env);
    }
    //enif_fprintf(stdout, "IMY************put111 \n");

    ErlNifBinary Key, Value;
    if (!enif_term_to_binary(env, argv[1], &Key))
        return enif_make_badarg(env);

    if (!enif_term_to_binary(env, argv[2], &Value))
        return enif_make_badarg(env);

    uint32_t BblIndex_1, BblIndex_2;
    BblIndex_1 = murmurhash(Key.data, Key.size, 131) % ResPtr->bblSize_1;
    BblIndex_2 = murmurhash(Key.data, Key.size, 16777619) % ResPtr->bblSize_2;
    //enif_fprintf(stdout, "IMY************put222  %d %d \n", BblIndex_1, BblIndex_2);

    //enif_fprintf(stdout, "IMY************put3333 \n");
    if (NULL == ResPtr->arrayPtr[BblIndex_1][BblIndex_2].head) {
        node *NewNode = enif_alloc(sizeof(node));
        NewNode->Key = Key;
        NewNode->Value = Value;
        NewNode->next = NULL;
        ResPtr->arrayPtr[BblIndex_1][BblIndex_2].head = NewNode;
        ResPtr->keySize = ResPtr->keySize + 1;
        //enif_fprintf(stdout, "IMY************put4444 \n");
        return ok;
    } else {
        node *Head = ResPtr->arrayPtr[BblIndex_1][BblIndex_2].head;
        while (Head) {
            if (compareBin(Key, Head->Key) == 0) {
                //ERL_NIF_TERM Ret;
                ErlNifBinary OldValue = Head->Value;
                //enif_binary_to_term(env, OldValue.data, OldValue.size, &Ret, 0);
                Head->Value = Value;
                enif_release_binary(&OldValue);
                enif_release_binary(&Key);
                //enif_fprintf(stdout, "IMY************put55555 \n");
                //return Ret;
                return ok;
            }
            Head = Head->next;
        }
        node *NewNode = enif_alloc(sizeof(node));
        NewNode->Key = Key;
        NewNode->Value = Value;
        NewNode->next = ResPtr->arrayPtr[BblIndex_1][BblIndex_2].head;
        ResPtr->arrayPtr[BblIndex_1][BblIndex_2].head = NewNode;
        ResPtr->keySize = ResPtr->keySize + 1;
        //enif_fprintf(stdout, "IMY************put6666\n");
        return ok;
    }
}
/////////////////////////////////////////////////////////////////////////

static ErlNifFunc nifFuns[] = {
        {"new",         0, new},
        {"get",         2, get},
        {"put",         3, put},
        {"hash1",       2, hash1},
        {"hash2",       2, hash2},
        {"hash3",       2, hash3},
        {"compareBin1", 2, compareBin1},
        {"compareBin2", 2, compareBin2},
};

ERL_NIF_INIT(nifHashb, nifFuns,
&load, NULL, NULL, NULL)


