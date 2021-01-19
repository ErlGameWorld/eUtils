#include "erl_nif.h"
#include <stdio.h>

static ERL_NIF_TERM getBinAddr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    enif_inspect_binary(env, argv[0], &bin);
    char buf[256];
    sprintf(buf, "bin: size=%zu, ptr=%p", bin.size, bin.data);
    return enif_make_string(env, buf, ERL_NIF_LATIN1);
}
static ErlNifFunc nif_funcs[] = {
    {"getBinAddr", 1, getBinAddr}
};

ERL_NIF_INIT(binaryAddr, nif_funcs, NULL, NULL, NULL, NULL);