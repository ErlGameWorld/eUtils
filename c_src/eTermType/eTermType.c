#include "erl_nif.h"
#include <stdio.h>

static ERL_NIF_TERM termType(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifTermType Type = enif_term_type(env, argv[0]);
    return enif_make_int(env, Type);
}
static ErlNifFunc nif_funcs[] = {
    {"termType", 1, termType}
};

ERL_NIF_INIT(eTermType, nif_funcs, NULL, NULL, NULL, NULL);