//
// Created by fox on 2017/10/10.
//

#ifndef NIF_SKIPLIST_SKIPLIST_H
#define NIF_SKIPLIST_SKIPLIST_H

#ifdef USENIF
#define imalloc enif_alloc
#define icalloc enif_alloc
#define irealloc enif_realloc
#define ifree enif_free
#else
#define imalloc malloc
#define icalloc(x) calloc(x, 1)
#define irealloc realloc
#define ifree free
#endif

typedef int vtype;

typedef struct snode {
    int score;
    int value;
    struct skiplistLevel {
        struct snode *forward;
        unsigned int span;
    } level[];
} snode;

typedef struct skiplist {
    int level;
    int size;
    struct snode *header;
} skiplist;

typedef struct skiplist_search_ret {
    int index;        // start from 1
    snode *node;
} skiplist_search_ret;

void test_skiplist(void);

// functions
skiplist *skiplist_init(void);
void skiplist_free(skiplist *list);
int skiplist_insert(skiplist *list, int score, vtype value);
int skiplist_update(skiplist *list, int score, vtype value, int old_score);
int skiplist_delete(skiplist *list, int score, vtype value);
void skiplist_search(skiplist *list, int score, skiplist_search_ret *ret);
int skiplist_index_of_score(skiplist *list, int score);
snode *skiplist_at(skiplist *list, int index);


#endif //NIF_SKIPLIST_SKIPLIST_H
