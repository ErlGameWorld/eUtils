#include <stdint.h>
#include "erl_nif.h"

#ifdef _WIN32
#define INLINE __inline
#else
#define INLINE inline
#endif

static ErlNifResourceType* ResType = NULL;
ERL_NIF_TERM null;
ERL_NIF_TERM ok;

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ResType = enif_open_resource_type(env, NULL, "_nifArray_", NULL, flags, NULL);
    if(NULL == ResType)
        return -1;
    null = enif_make_atom(env, "null");
    ok = enif_make_atom(env, "ok");
    return 0;
}

static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint32_t Size;
    if(!enif_get_uint(env, argv[0], &Size)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary *ArrayPtr = (ErlNifBinary *) enif_alloc_resource(ResType, sizeof(ErlNifBinary)*Size);
    int i;
    for(i = 0; i < Size; i++){
        ErlNifBinary nullBin;
        enif_term_to_binary(env, null, &nullBin);
        ArrayPtr[i] = nullBin;
    }
    ERL_NIF_TERM Res = enif_make_resource(env, ArrayPtr);
    enif_release_resource(ArrayPtr);
    return Res;
}

static ERL_NIF_TERM get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint32_t Index;
    if(!enif_get_uint(env, argv[1], &Index)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary *ArrayPtr;

    if (!enif_get_resource(env, argv[0], ResType, (void **)&ArrayPtr)){
        return enif_make_badarg(env);
    }
    //enif_fprintf(stdout, "IMY************get  %T \n", argv[1]);
    ERL_NIF_TERM Ret;
    enif_binary_to_term(env, ArrayPtr[Index].data, ArrayPtr[Index].size, &Ret, 0);
    return Ret;
}

static ERL_NIF_TERM put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    //enif_fprintf(stdout, "IMY************put0001  %T \n", argv[1]);
    uint32_t Index;
    if(!enif_get_uint(env, argv[1], &Index)) {
        return enif_make_badarg(env);
    }
    //enif_fprintf(stdout, "IMY************put0002  %T  %T \n", argv[1], argv[2]);
    ErlNifBinary Value;
    if(!enif_term_to_binary(env, argv[2], &Value))
        return enif_make_badarg(env);

    ErlNifBinary *ArrayPtr;
    //enif_fprintf(stdout, "IMY************put0003  %T \n", argv[1]);
    if (!enif_get_resource(env, argv[0], ResType, (void **)&ArrayPtr)){
        return enif_make_badarg(env);
    }
    //enif_fprintf(stdout, "IMY************put111  %T \n", argv[1]);
    ErlNifBinary OldValue = ArrayPtr[Index];
   // enif_fprintf(stdout, "IMY************put222  %T %d \n", argv[1], Index);
    ArrayPtr[Index] = Value;
    //enif_fprintf(stdout, "IMY************put333  %T \n", argv[1]);
    enif_release_binary(&OldValue);
    //enif_fprintf(stdout, "IMY************put444  %T \n", argv[1]);
    return ok;
}

static ERL_NIF_TERM test(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary Value;
    if(!enif_term_to_binary(env, argv[0], &Value))
        return enif_make_badarg(env);
    // enif_fprintf(stdout, "IMY************  %T", argv[0]);
    ERL_NIF_TERM Ret;
    enif_binary_to_term(env, Value.data, Value.size, &Ret, 0);
    enif_release_binary(&Value);
    return Ret;
}

static ErlNifFunc nifFuns[] = {
        {"new", 1, new},
        {"get", 2, get},
        {"put", 3, put},
        {"test", 1, test},
};

ERL_NIF_INIT(nifArray, nifFuns, &load, NULL, NULL, NULL)
