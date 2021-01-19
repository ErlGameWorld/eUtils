// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "hqueue.h"


typedef struct
{
    ERL_NIF_TERM atom_ok;
    ERL_NIF_TERM atom_error;
    ERL_NIF_TERM atom_value;
    ERL_NIF_TERM atom_empty;
    ERL_NIF_TERM atom_full;
    ERL_NIF_TERM atom_max_elems;
    ERL_NIF_TERM atom_heap_size;
    ERL_NIF_TERM atom_too_small;
    ErlNifResourceType* res_hqueue;
} hqueue_priv;


typedef struct
{
    ErlNifEnv* env;
    ERL_NIF_TERM value;
} hqnode_nif_t;


typedef struct
{
    int version;
    uint64_t gen;
    hqueue_t* hqueue;
    ErlNifPid p;
} hqueue_nif_t;


static const uint32_t default_max_elems = UINT32_MAX-1;
static const uint32_t default_heap_size = 1024;


static inline ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}


static inline ERL_NIF_TERM
make_ok(ErlNifEnv* env, hqueue_priv* priv, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, priv->atom_ok, value);
}


static inline ERL_NIF_TERM
make_error(ErlNifEnv* env, hqueue_priv* priv, ERL_NIF_TERM reason)
{
    return enif_make_tuple2(env, priv->atom_error, reason);
}


static inline int
check_pid(ErlNifEnv* env, hqueue_nif_t* hqueue_nif)
{
    ErlNifPid pid;
    enif_self(env, &pid);

    if(enif_compare(pid.pid, hqueue_nif->p.pid) == 0) {
        return 1;
    }

    return 0;
}


void
hqueue_nif_node_free(hqnode_nif_t* hqnode_nif)
{
    enif_free_env(hqnode_nif->env);
    enif_free(hqnode_nif);

    return;
}


void
hqueue_nif_node_free_ext(void* node)
{
    hqueue_nif_node_free((hqnode_nif_t*) node);

    return;
}


hqnode_nif_t*
hqueue_nif_node_alloc()
{
    hqnode_nif_t* node = (hqnode_nif_t*) enif_alloc(sizeof(hqnode_nif_t*));

    memset(node, 0, sizeof(hqnode_nif_t));

    node->env = enif_alloc_env();

    return node;
}


static int
get_uint_param(ErlNifEnv* env, ERL_NIF_TERM value, ERL_NIF_TERM atom, uint32_t* p)
{
    const ERL_NIF_TERM* tuple;
    int arity;

    if(!enif_get_tuple(env, value, &arity, &tuple)) {
        return 0;
    }

    if(arity != 2) {
        return 0;
    }

    if(enif_compare(tuple[0], atom) != 0) {
        return 0;
    }

    if(!enif_get_uint(env, tuple[1], p)) {
        return 0;
    }

    return 1;
}


static inline hqueue_nif_t*
hqueue_nif_create_int(ErlNifEnv* env, hqueue_priv* priv, uint32_t max_elems,
        uint32_t heap_size)
{
    hqueue_nif_t* hqueue_nif = NULL;

    assert(priv != NULL && "missing private data member");

    hqueue_nif = (hqueue_nif_t*) enif_alloc_resource(
            priv->res_hqueue, sizeof(hqueue_nif_t));
    memset(hqueue_nif, 0, sizeof(hqueue_nif_t));
    hqueue_nif->version = HQ_VERSION;

    hqueue_nif->hqueue = hqueue_new(max_elems, heap_size);

    if(hqueue_nif->hqueue == NULL ) {
        enif_release_resource(hqueue_nif);
        return NULL;
    }

    enif_self(env, &(hqueue_nif->p));

    return hqueue_nif;
}


static ERL_NIF_TERM
hqueue_nif_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    ERL_NIF_TERM ret;
    ERL_NIF_TERM opts;
    ERL_NIF_TERM value;
    uint32_t max_elems = default_max_elems;
    uint32_t heap_size = default_heap_size;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    opts = argv[0];
    if(!enif_is_list(env, opts)) {
        return enif_make_badarg(env);
    }

    while(enif_get_list_cell(env, opts, &value, &opts)) {
        if(get_uint_param(env, value, priv->atom_max_elems, &max_elems)) {
            continue;
        } else if(get_uint_param(env, value, priv->atom_heap_size, &heap_size)) {
            continue;
        } else {
            return enif_make_badarg(env);
        }
    }

    hqueue_nif = hqueue_nif_create_int(env, priv, max_elems, heap_size);
    if(hqueue_nif == NULL) {
        return enif_make_badarg(env);
    }

    ret = enif_make_resource(env, hqueue_nif);
    enif_release_resource(hqueue_nif);

    return make_ok(env, priv, ret);
}


static void
hqueue_nif_free(ErlNifEnv* env, void* obj)
{
    hqueue_nif_t* hqueue_nif = (hqueue_nif_t*) obj;

    hqueue_free2(hqueue_nif->hqueue, hqueue_nif_node_free_ext);

    return;
}


static ERL_NIF_TERM
hqueue_nif_extract_max(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    hqnode_nif_t* hqnode_nif;
    double tmp_priority;
    ERL_NIF_TERM ret;
    ERL_NIF_TERM priority;
    ERL_NIF_TERM value;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hqueue, (void**) &hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if (!hqueue_extract_max(hqueue_nif->hqueue, &tmp_priority, (void**) &hqnode_nif)) {
        return make_error(env, priv, priv->atom_empty);
    }

    priority = enif_make_double(env, tmp_priority);
    value = enif_make_copy(env, hqnode_nif->value);
    ret = enif_make_tuple2(env, priority, value);

    hqueue_nif_node_free(hqnode_nif);

    return ret;
}


static ERL_NIF_TERM
hqueue_nif_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    hqnode_nif_t* hqnode_nif;
    ERL_NIF_TERM ret;
    double priority;

    if(argc != 3) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hqueue, (void**) &hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_double(env, argv[1], &priority)) {
        return enif_make_badarg(env);
    }

    if(priority < 0.0) {
        return enif_make_badarg(env);
    }

    hqnode_nif = hqueue_nif_node_alloc();
    hqnode_nif->value = enif_make_copy(hqnode_nif->env, argv[2]);

    if (!hqueue_insert(hqueue_nif->hqueue, priority, (void*) hqnode_nif)) {
        return make_error(env, priv, priv->atom_full);
    }

    ret = priv->atom_ok;

    return ret;
}


static ERL_NIF_TERM
hqueue_nif_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    ERL_NIF_TERM ret;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hqueue, (void**) &hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, hqueue_nif)) {
        return enif_make_badarg(env);
    }

    ret = enif_make_uint64(env, hqueue_size(hqueue_nif->hqueue));

    return ret;
}


static ERL_NIF_TERM
hqueue_nif_heap_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    ERL_NIF_TERM ret;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hqueue, (void**) &hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, hqueue_nif)) {
        return enif_make_badarg(env);
    }

    ret = enif_make_uint64(env, hqueue_heap_size(hqueue_nif->hqueue));

    return ret;
}


static ERL_NIF_TERM
hqueue_nif_max_elems(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    ERL_NIF_TERM ret;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hqueue, (void**) &hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, hqueue_nif)) {
        return enif_make_badarg(env);
    }

    ret = enif_make_uint64(env, hqueue_max_elems(hqueue_nif->hqueue));

    return ret;
}


static ERL_NIF_TERM
hqueue_nif_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    hqueue_t* hqueue;
    hqnode_nif_t* hqnode_nif;
    double tmp_priority;
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    ERL_NIF_TERM priority;
    ERL_NIF_TERM value;
    ERL_NIF_TERM tuple;
    uint32_t i;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hqueue, (void**) &hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, hqueue_nif)) {
        return enif_make_badarg(env);
    }

    hqueue = hqueue_nif->hqueue;

    for (i = 1; i <= hqueue_size(hqueue); i++) {
        hqueue_get_elem(hqueue, i, &tmp_priority, (void **) &hqnode_nif);
        priority = enif_make_double(env, tmp_priority);
        value = enif_make_copy(env, hqnode_nif->value);
        tuple = enif_make_tuple2(env, priority, value);
        ret = enif_make_list_cell(env, tuple, ret);
    }

    return ret;
}


static ERL_NIF_TERM
hqueue_nif_scale_by(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    ERL_NIF_TERM ret;
    double factor;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hqueue, (void**) &hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_double(env, argv[1], &factor)) {
        return enif_make_badarg(env);
    }

    if(factor < 0.0) {
        return enif_make_badarg(env);
    }

    hqueue_scale_by(hqueue_nif->hqueue, factor);

    ret = priv->atom_ok;

    return ret;
}


static ERL_NIF_TERM
hqueue_nif_resize_heap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    ERL_NIF_TERM ret;
    uint32_t new_heap_size;
    uint32_t old_heap_size;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hqueue, (void**) &hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[1], &new_heap_size)) {
        return enif_make_badarg(env);
    }

    if(hqueue_size(hqueue_nif->hqueue) > new_heap_size) {
        return make_error(env, priv, priv->atom_too_small);
    }

    if((old_heap_size = hqueue_resize_heap(hqueue_nif->hqueue, new_heap_size)) == 0) {
        return enif_make_badarg(env);
    }

    ret = enif_make_uint64(env, old_heap_size);

    return ret;
}


static ERL_NIF_TERM
hqueue_nif_set_max_elems(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hqueue_priv* priv = enif_priv_data(env);
    hqueue_nif_t* hqueue_nif;
    ERL_NIF_TERM ret;
    uint32_t new_max_elems;
    uint32_t old_max_elems;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hqueue, (void**) &hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, hqueue_nif)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[1], &new_max_elems)) {
        return enif_make_badarg(env);
    }

    if(hqueue_size(hqueue_nif->hqueue) > new_max_elems) {
        return make_error(env, priv, priv->atom_too_small);
    }

    if ((old_max_elems = hqueue_set_max_elems(hqueue_nif->hqueue, new_max_elems)) == 0) {
        return enif_make_badarg(env);
    }

    ret = enif_make_uint64(env, old_max_elems);

    return ret;
}


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* res;

    hqueue_priv* new_priv = (hqueue_priv*) enif_alloc(sizeof(hqueue_priv));
    if(new_priv == NULL) {
        return 1;
    }

    res = enif_open_resource_type(
            env, NULL, "hqueue", hqueue_nif_free, flags, NULL);
    if(res == NULL) {
        enif_free(new_priv);
        return 1;
    }
    new_priv->res_hqueue = res;

    new_priv->atom_ok = make_atom(env, "ok");
    new_priv->atom_error = make_atom(env, "error");
    new_priv->atom_value = make_atom(env, "value");
    new_priv->atom_empty = make_atom(env, "empty");
    new_priv->atom_full = make_atom(env, "full");
    new_priv->atom_max_elems = make_atom(env, "max_elems");
    new_priv->atom_heap_size = make_atom(env, "heap_size");
    new_priv->atom_too_small = make_atom(env, "too_small");

    *priv = (void*) new_priv;

    return 0;
}


static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}


static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}


static ErlNifFunc funcs[] = {
    {"new", 1, hqueue_nif_new},
    {"extract_max", 1, hqueue_nif_extract_max},
    {"insert", 3, hqueue_nif_insert},
    {"size", 1, hqueue_nif_size},
    {"heap_size", 1, hqueue_nif_heap_size},
    {"max_elems", 1, hqueue_nif_max_elems},
    {"set_max_elems", 2, hqueue_nif_set_max_elems},
    {"to_list", 1, hqueue_nif_to_list},
    {"scale_by", 2, hqueue_nif_scale_by},
    {"resize_heap", 2, hqueue_nif_resize_heap}
};


ERL_NIF_INIT(hqueue, funcs, &load, NULL, &upgrade, &unload);
