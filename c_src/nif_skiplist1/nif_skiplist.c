//
// Created by fox on 2017/8/10.
//

#ifndef USENIF
#define USENIF 1
#endif

#include <string.h>
#include "erl_nif.h"
#include "skiplist.h"

// all pointer to skiplist would be stored here
static skiplist **p_array;
static size_t p_array_size = 0;

#define enif_get_value enif_get_int
#define enif_make_value enif_make_int

// store a new pointer
int store_new_pointer(skiplist *p)
{
    if (p_array_size == 0) {
        // init it
        p_array_size = 2;
        p_array = (skiplist **)icalloc(p_array_size * sizeof(skiplist *));
        p_array[0] = p;

        return 0;
    }

    // find the empty pos
    for (int i = 0; i < p_array_size; ++i) {
        if (p_array[i] == NULL) {
            p_array[i] = p;
            return i;
        }
    }

    // reallocate array
    size_t old_size = p_array_size;
    p_array_size *= 2;

    p_array = (skiplist **)realloc(p_array, sizeof(skiplist *) * p_array_size);
    memset(p_array+old_size, 0, sizeof(skiplist *) * old_size);

    p_array[old_size] = p;
    return (int)old_size;
}

inline skiplist *get_pointer(ErlNifEnv* env, ERL_NIF_TERM arg);
skiplist *get_pointer(ErlNifEnv* env, ERL_NIF_TERM arg)
{
    int index;
    if (!enif_get_int(env, arg, &index))
        return NULL;

    if (0 <= index && index < p_array_size)
        return p_array[index];

    return NULL;
}

static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    skiplist *list = skiplist_init();
    int pointer_pos = store_new_pointer(list);

    ERL_NIF_TERM res = enif_make_int(env, pointer_pos);
    return res;
}

static ERL_NIF_TERM free1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int index;
    if (!enif_get_int(env, argv[0], &index))
        return enif_make_badarg(env);

    if (index < 0 || index >= p_array_size)
        return enif_make_badarg(env);

    skiplist_free(p_array[index]);
    p_array[index] = NULL;

    ERL_NIF_TERM res;
    enif_make_existing_atom(env, "ok", &res, ERL_NIF_LATIN1);
    return res;
}

static ERL_NIF_TERM insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    skiplist *list = get_pointer(env, argv[0]);
    if (!list)
        return enif_make_badarg(env);

    int score;
    vtype value;
    if (!enif_get_int(env, argv[1], &score))
        return enif_make_badarg(env);
    if (!enif_get_value(env, argv[2], &value))
        return enif_make_badarg(env);

    int res0 = skiplist_insert(list, score, value);

    ERL_NIF_TERM res = enif_make_int(env, res0);
    return res;
}

static ERL_NIF_TERM update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    skiplist *list = get_pointer(env, argv[0]);
    if (!list)
        return enif_make_badarg(env);

    int score, old_score;
    vtype value;
    if (!enif_get_int(env, argv[1], &score))
        return enif_make_badarg(env);
    if (!enif_get_value(env, argv[2], &value))
        return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &old_score))
        return enif_make_badarg(env);

    int res0 = skiplist_update(list, score, value, old_score);

    ERL_NIF_TERM res = enif_make_int(env, res0);
    return res;
}

static ERL_NIF_TERM delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    skiplist *list = get_pointer(env, argv[0]);
    if (!list)
        return enif_make_badarg(env);

    int socre;
    vtype value;
    if (!enif_get_int(env, argv[1], &socre))
        return enif_make_badarg(env);
    if (!enif_get_value(env, argv[2], &value))
        return enif_make_badarg(env);

    int res0 = skiplist_delete(list, socre, value);

    ERL_NIF_TERM res = enif_make_int(env, res0);
    return res;
}

static ERL_NIF_TERM to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    skiplist *list = get_pointer(env, argv[0]);
    if (!list)
        return enif_make_badarg(env);

    snode *x = list->header->level[0].forward;
    snode **nodes = (snode **)malloc(sizeof(snode *) * (list->size + 1));

    while (x) {
        *nodes = x;
        ++nodes;
        x = x->level[0].forward;
    }

    ERL_NIF_TERM res = enif_make_list(env, 0);
    for (int i = list->size-1; i >= 0; --i) {
        --nodes;
        ERL_NIF_TERM item = enif_make_tuple(
                env,
                2,
                enif_make_int(env, (*nodes)->score),
                enif_make_value(env, (*nodes)->value));
        res = enif_make_list_cell(env, item, res);
    }

    free(nodes);

    return res;
}

static ERL_NIF_TERM size1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    skiplist *list = get_pointer(env, argv[0]);
    if (!list)
        return enif_make_badarg(env);

    ERL_NIF_TERM res = enif_make_int(env, list->size);
    return res;
}

// first index of the score
static ERL_NIF_TERM index_of_score(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
    skiplist *list = get_pointer(env, argv[0]);
    if (!list)
        return enif_make_badarg(env);

    int socre;
    if (!enif_get_int(env, argv[1], &socre))
        return enif_make_badarg(env);

    int res0 = skiplist_index_of_score(list, socre);
    ERL_NIF_TERM res = enif_make_int(env, res0);

    return res;
}

static ERL_NIF_TERM at(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    skiplist *list = get_pointer(env, argv[0]);
    if (!list)
        return enif_make_badarg(env);

    int index;
    if (!enif_get_int(env, argv[1], &index))
        return enif_make_badarg(env);

    snode *x = skiplist_at(list, index);

    ERL_NIF_TERM res;

    if (x) {
        res = enif_make_tuple(
                env,
                2,
                enif_make_int(env, x->score),
                enif_make_value(env, x->value));
    } else {
        enif_make_existing_atom(env, "error", &res, ERL_NIF_LATIN1);
    }

    return res;
}

static ERL_NIF_TERM range_by_score(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    skiplist *list = get_pointer(env, argv[0]);
    if (!list)
        return enif_make_badarg(env);

    int score1, score2;
    if (!enif_get_int(env, argv[1], &score1))
        return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &score2))
        return enif_make_badarg(env);

    skiplist_search_ret res0;
    skiplist_search(list, score1, &res0);

    ERL_NIF_TERM ret_list = enif_make_list(env, 0);
    for (snode *node = res0.node; node && node->score <= score2; node = node->level[0].forward) {
        ret_list = enif_make_list_cell(env,
                                       enif_make_tuple(env,
                                                       2,
                                                       enif_make_int(env, node->score),
                                                       enif_make_value(env, node->value)),
                                       ret_list);
    }
    ERL_NIF_TERM ret_list1;
    enif_make_reverse_list(env, ret_list, &ret_list1);

    return enif_make_tuple(
            env,
            2,
            enif_make_int(env, res0.index),
            ret_list1);
}

static ERL_NIF_TERM range_1(
        ErlNifEnv* env,
        const ERL_NIF_TERM argv[],
        ERL_NIF_TERM (*func_make_item)(ErlNifEnv *, snode *))
{
    skiplist *list = get_pointer(env, argv[0]);
    if (!list)
        return enif_make_badarg(env);

    int start, len;
    if (!enif_get_int(env, argv[1], &start))
        return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &len))
        return enif_make_badarg(env);

    int n = list->size - start + 1;
    int alloc_size = n < len ? n : len;

    ERL_NIF_TERM res = enif_make_list(env, 0);
    if (alloc_size <= 0) {
        return res;
    }

    snode **nodes = (snode **)malloc(sizeof(snode *) * (alloc_size + 1));

    // get the nodes
    snode *x = skiplist_at(list, start);
    for (n = 0; n < alloc_size; n++) {
        *nodes = x;
        ++nodes;
        x = x->level[0].forward;
    }

    for (n = 0; n < alloc_size; n++) {
        --nodes;
        res = enif_make_list_cell(env, func_make_item(env, *nodes), res);
    }

    free(nodes);

    return res;
}

inline static ERL_NIF_TERM range_make_item(ErlNifEnv* env, snode *node);
static ERL_NIF_TERM range_make_item(ErlNifEnv* env, snode *node)
{
    return enif_make_value(env, node->value);
}

/* int p_list: identification of skiplist
 * int start: >= 1, start index of skiplist
 * int len: >= 1, max items to return
 * return: [node]
 */
static ERL_NIF_TERM range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return range_1(env, argv, range_make_item);
}

inline static ERL_NIF_TERM range_with_score_make_item(ErlNifEnv* env, snode *node);
static ERL_NIF_TERM range_with_score_make_item(ErlNifEnv* env, snode *node)
{
    return enif_make_tuple(
            env,
            2,
            enif_make_int(env, node->score),
            enif_make_value(env, node->value));
}

/* int p_list: identification of skiplist
 * int start: >= 1, start index of skiplist
 * int len: >= 1, max items to return
 * return: [{score, node}]
 */
static ERL_NIF_TERM range_with_score(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return range_1(env, argv, range_with_score_make_item);
}

static ErlNifFunc nif_funcs[] = {
        {"new", 0, new},
        {"free", 1, free1},
        {"insert", 3, insert},
        {"update", 4, update},
        {"delete", 3, delete},
        {"to_list", 1, to_list},
        {"size", 1, size1},
        {"index_of_score", 2, index_of_score},
        {"at", 2, at},
        {"range", 3, range},
        {"range_with_score", 3, range_with_score},
        {"range_by_score", 3, range_by_score},
};

ERL_NIF_INIT(skiplist, nif_funcs, NULL, NULL, NULL, NULL)


