#include <string.h>
#include "erl_nif.h"
#include "concurrentqueue.h"

struct qData {
    ErlNifEnv *env;
    ERL_NIF_TERM term;
};

struct lfqIns {
    moodycamel::ConcurrentQueue<qData> *LFQ;
    moodycamel::ConcurrentQueue<ErlNifEnv *> *G_ENV;
};

ERL_NIF_TERM atomOk;
ERL_NIF_TERM atomError;
ERL_NIF_TERM atomTrue;
ERL_NIF_TERM atomFalse;
ERL_NIF_TERM atomEmpty;

ERL_NIF_TERM make_binary(ErlNifEnv *env, const char *buff, size_t length) {
    ERL_NIF_TERM term;
    unsigned char *destination_buffer = enif_make_new_binary(env, length, &term);
    memcpy(destination_buffer, buff, length);
    return term;
}

ERL_NIF_TERM make_error(ErlNifEnv *env, const char *error) {
    return enif_make_tuple2(env, atomError, make_binary(env, error, strlen(error)));
}

void eLfqFree(ErlNifEnv *, void *obj) {
    lfqIns *ObjIns = static_cast<lfqIns *>(obj);

    if (ObjIns != nullptr) {
        qData Data;
        while (ObjIns->LFQ->try_dequeue(Data)) {
            enif_free_env(Data.env);
        }

        ErlNifEnv *StoreEnv;
        while (ObjIns->G_ENV->try_dequeue(StoreEnv)) {
            enif_free_env(StoreEnv);
        }
        delete ObjIns->LFQ;
        delete ObjIns->G_ENV;
    }
}

int nifLoad(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM) {
    ErlNifResourceFlags flags = static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    ErlNifResourceType *ResIns;
    ResIns = enif_open_resource_type(env, NULL, "eLfqRes", NULL, flags, NULL);
    if (NULL == ResIns)
        return -1;

    *priv_data = ResIns;
    atomOk = enif_make_atom(env, "ok");
    atomError = enif_make_atom(env, "error");
    atomTrue = enif_make_atom(env, "true");
    atomFalse = enif_make_atom(env, "false");
    atomEmpty = enif_make_atom(env, "empty");

    return 0;
}

int nifUpgrade(ErlNifEnv *env, void **priv_data, void **, ERL_NIF_TERM) {
    ErlNifResourceFlags flags = static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    ErlNifResourceType *ResIns;
    ResIns = enif_open_resource_type(env, NULL, "eLfqRes", NULL, flags, NULL);
    if (NULL == ResIns)
        return -1;

    *priv_data = ResIns;
    return 0;
}

void nifUnload(ErlNifEnv *, void *priv_data) {
    return;
}

ERL_NIF_TERM nifNew(ErlNifEnv *env, int, const ERL_NIF_TERM *) {
    ErlNifResourceType *ResIns = static_cast<ErlNifResourceType *>(enif_priv_data(env));

    lfqIns *ObjIns = static_cast<lfqIns *>(enif_alloc_resource(ResIns, sizeof(lfqIns)));
    ObjIns->LFQ = new moodycamel::ConcurrentQueue<qData>;
    ObjIns->G_ENV = new moodycamel::ConcurrentQueue<ErlNifEnv *>;

    if (ObjIns == NULL)
        return make_error(env, "enif_alloc_resource failed");

    ERL_NIF_TERM term = enif_make_resource(env, ObjIns);
    enif_release_resource(ResIns);
    return enif_make_tuple2(env, atomOk, term);
}

ERL_NIF_TERM nifIn2(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType *ResIns = static_cast<ErlNifResourceType *>(enif_priv_data(env));

    lfqIns *ObjIns = NULL;

    if (!enif_get_resource(env, argv[0], ResIns, (void **) &ObjIns)) {
        return enif_make_badarg(env);
    }

    qData InTerm;

    InTerm.env = enif_alloc_env();
    InTerm.term = enif_make_copy(InTerm.env, argv[1]);

    if (ObjIns->LFQ->enqueue(InTerm)){
        return atomTrue;
    } else {
        return atomFalse;
    }
}

// ERL_NIF_TERM nifIn2(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
//     ErlNifResourceType* ResIns = static_cast<ErlNifResourceType *>(enif_priv_data(env));
//
//     lfqIns *ObjIns = NULL;
//
//     if (!enif_get_resource(env, argv[0], ResIns, (void **) &ObjIns)) {
//         return enif_make_badarg(env);
//     }
//
//     qData InTerm;
//     if (ObjIns->G_ENV->try_dequeue(InTerm.env) != true) {
//         //enif_fprintf(stdout, "IMY************\n");
//         InTerm.env = enif_alloc_env();
//     }
//
//     InTerm.term = enif_make_copy(InTerm.env, argv[1]);
//
//     if (ObjIns->LFQ->enqueue(InTerm)){
//         return atomTrue;
//     } else {
//         return atomFalse;
//     }
// }


ERL_NIF_TERM nifIn3(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomTrue;
}

ERL_NIF_TERM nifIns2(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomTrue;
}

ERL_NIF_TERM nifIns3(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomTrue;
}

ERL_NIF_TERM nifTryIn2(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomTrue;
}

ERL_NIF_TERM nifTryIn3(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomTrue;
}

ERL_NIF_TERM nifTryIns2(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomTrue;
}

ERL_NIF_TERM nifTryIns3(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomTrue;
}

ERL_NIF_TERM nifTryOut1(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType *ResIns = static_cast<ErlNifResourceType *>(enif_priv_data(env));
    lfqIns *ObjIns = NULL;

    if (!enif_get_resource(env, argv[0], ResIns, (void **) &ObjIns)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM OutTerm;
    qData Data;

    if (ObjIns->LFQ->try_dequeue(Data)) {
        OutTerm = enif_make_copy(env, Data.term);
        enif_free_env(Data.env);
        return OutTerm;
    } else {
        return atomEmpty;
    }
}

// ERL_NIF_TERM nifTryOut1(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
//     ErlNifResourceType* ResIns = static_cast<ErlNifResourceType *>(enif_priv_data(env));
//     lfqIns *ObjIns = NULL;
//
//     if (!enif_get_resource(env, argv[0], ResIns, (void **) &ObjIns)) {
//         return enif_make_badarg(env);
//     }
//
//     ERL_NIF_TERM OutTerm;
//     qData Data;
//
//     if (ObjIns->LFQ->try_dequeue(Data)) {
//         OutTerm = enif_make_copy(env, Data.term);
//
//         if(ObjIns->G_ENV->size_approx() > 1000){
//             enif_free_env(Data.env);
//         }else{
//             enif_clear_env(Data.env);
//             if (!ObjIns->G_ENV->enqueue(Data.env)){
//                 enif_free_env(Data.env);
//             }
//         }
//         return enif_make_tuple2(env, atomOk, OutTerm);
//     } else {
//         return atomEmpty;
//     }
// }

ERL_NIF_TERM nifTryOut2(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomEmpty;
}

ERL_NIF_TERM nifTryOuts2(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomEmpty;
}

ERL_NIF_TERM nifTryOuts3(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomEmpty;
}

ERL_NIF_TERM nifTryOutByProd2(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomEmpty;
}

ERL_NIF_TERM nifTryOutByProd3(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomEmpty;
}

ERL_NIF_TERM nifSize1(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    return atomEmpty;
}


static ErlNifFunc nifFuncs[] =
        {
                {"new",           0, nifNew},
                {"in",            2, nifIn2},
                {"in",            3, nifIn3},
                {"ins",           2, nifIns2},
                {"ins",           3, nifIns3},
                {"tryIn",         2, nifTryIn2},
                {"tryIn",         3, nifTryIn3},
                {"tryIns",        2, nifTryIns2},
                {"tryIns",        3, nifTryIns3},
                {"tryOut",        1, nifTryOut1},
                {"tryOut",        2, nifTryOut2},
                {"tryOuts",       2, nifTryOuts2},
                {"tryOuts",       3, nifTryOuts3},
                {"tryOutByProd",  2, nifTryOutByProd2},
                {"tryOutsByProd", 3, nifTryOutByProd3},
                {"size",          1, nifSize1}
        };

ERL_NIF_INIT(eLfq, nifFuncs, nifLoad, NULL, nifUpgrade, nifUnload
)