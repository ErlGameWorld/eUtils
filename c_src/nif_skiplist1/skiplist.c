//
// Created by fox on 2017/10/10.
// Skip Lists: A Probabilistic Alternative to Balanced Trees
// indexable skip list
// duplicated score and node

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include "skiplist.h"
#include <time.h>

// from 0 to n
#define SKIPLIST_MAX_LEVEL 15
#define LEVEL_SIZE (SKIPLIST_MAX_LEVEL+1)
#define RAND_UNIFORM(x) (int)((double)rand() / RAND_MAX * (x))

/* Create a skiplist node with the specified number of levels. */
snode *skiplistCreateNode(int level, int score, vtype value) {
    snode *zn =
            (snode *)icalloc(sizeof(*zn)+level*sizeof(struct skiplistLevel));
    zn->score = score;
    zn->value = value;
    return zn;
}

skiplist *skiplist_init(void)
{
    time_t t;
    srand((unsigned)(time(&t)));

    snode *header = skiplistCreateNode(LEVEL_SIZE, INT_MAX, INT_MAX);
    for (int i = 0; i < LEVEL_SIZE; ++i) {
        header->level[i].span = 1;
    }

    skiplist *list = (skiplist *)imalloc(sizeof(skiplist));
    list->header = header;
    list->level = 0;
    list->size = 0;

    return list;
}

static int rand_level()
{
    int level = 0;
    while (rand() < RAND_MAX / 2 && level < SKIPLIST_MAX_LEVEL)
//    while (arc4random() < INT_MAX && level < SKIPLIST_MAX_LEVEL)
        level++;
    return level;
}

// insert score,node into a skiplist, return 0
int skiplist_insert(skiplist *list, int score, vtype value)
{
    snode **update = (snode **)imalloc(sizeof(snode*) * (list->level + 1));
    unsigned int *fore_width = (unsigned int *)imalloc(sizeof(unsigned int) * (list->level + 1));
    snode *x = list->header;
    int i;
    for (i = list->level; i >= 0; i--) {
        fore_width[i] = 0;
        while (x->level[i].forward && x->level[i].forward->score <= score) {
            fore_width[i] += x->level[i].span;
            x = x->level[i].forward;
        }
        update[i] = x;
    }
    // assert(x->score <= score);

    int level = rand_level();
    list->size = list->size+1;

    x = skiplistCreateNode(level+1, score, value);

    // the lowest layer is one by one
    x->level[0].forward = update[0]->level[0].forward;
    x->level[0].span = 1;
    update[0]->level[0].forward = x;

    unsigned int temp_width;
    for (i = 1; i <= (list->level < level ? list->level : level); i++) {
        temp_width = fore_width[i-1] + update[i-1]->level[i-1].span;
        x->level[i].forward = update[i]->level[i].forward;
        x->level[i].span = update[i]->level[i].span + 1 - temp_width;
        update[i]->level[i].forward = x;
        update[i]->level[i].span = temp_width;
    }

    if (level > list->level) {
        // complete the new level
        temp_width = fore_width[list->level] + update[list->level]->level[list->level].span;
        for (i = list->level+1; i <= level; i++) {
            list->header->level[i].span = temp_width;
            list->header->level[i].forward = x;
            x->level[i].forward = NULL;
            x->level[i].span = list->size + 1 - temp_width;
        }
        list->level = level;
    }
    else {
        // complete the unreached level
        for (; i <= list->level; ++i) {
            update[i]->level[i].span++;
        }
    }

    ifree(update);
    ifree(fore_width);
    return 0;
}

int skiplist_update(skiplist *list, int score, vtype value, int old_score)
{
    skiplist_delete(list, old_score, value);
    return skiplist_insert(list, score, value);
}

// search score in skiplist.
// set the struct with index and first node if exists, otherwise set index 0
void skiplist_search(skiplist *list, int score, skiplist_search_ret *ret)
{
    snode *x = list->header;
    int temp_width = 1;
    for (int i = list->level; i >= 0; i--) {
        while (x->level[i].forward && x->level[i].forward->score < score) {
            temp_width += x->level[i].span;
            x = x->level[i].forward;
        }
    }
    x = x->level[0].forward;

    ret->index = temp_width;
    ret->node = x;
}

// first index of score
int skiplist_index_of_score(skiplist *list, int score)
{
    snode *x = list->header;
    int temp_width = 1;
    for (int i = list->level; i >= 0; i--) {
        while (x->level[i].forward && x->level[i].forward->score < score) {

            temp_width += x->level[i].span;
            x = x->level[i].forward;
        }
    }
    x = x->level[0].forward;

    // check if existed score
    if (x && x->score == score) {
        return temp_width;
    }
    return 0;
}

// search skiplist by index. Return the node if exists, otherwise NULL
snode *skiplist_at(skiplist *list, int index)
{
    snode *x = list->header;
    for (int i = list->level; i >= 0; i--) {
        while (x->level[i].forward) {
            if (x->level[i].span == index) {
                return x->level[i].forward;
            }
            if (x->level[i].span < index) {
                index -= x->level[i].span;
                x = x->level[i].forward;
            }
            else {
                break;
            }
        }
    }

    return NULL;
}

static void skiplist_node_free(snode *x)
{
    if (x) {
        ifree(x);
    }
}

// delete by score,node. Return 0 if success, 1 if fail.
int skiplist_delete(skiplist *list, int score, vtype value)
{
    int i;
    snode **update = (snode **)imalloc(sizeof(snode*) * (list->level + 1));
    snode *x = list->header;

    // find every level before the specified node
    for (i = list->level; i >= 0; --i) {
        while (1) {
            if (!(x->level[i].forward) || x->level[i].forward->score > score) {
                update[i] = x;
                break;
            }

            if (x->level[i].forward->score < score) {
                x = x->level[i].forward;
                continue;
            }

            // find the first node with same score
            int j;
            update[i] = x;
            for (j = i-1; j >= 0; --j) {
                while (x->level[j].forward->score < score)
                    x = x->level[j].forward;

                update[j] = x;
            }
            x = x->level[0].forward;
            snode *x_start_search = x;

            // find the first node with same score and node
            while (x && x->value != value && x->score == score) {
                x = x->level[0].forward;
            }
            if (x && x->score == score) {
                // now x is the node to find
                // find nodes for every level before the node to find
                if (x == x_start_search) {
                    // done
                    i = 0;
                    break;
                }

                // j is used to judge if change the x_start_search
                j = 0;
                snode *iter;
                for (; i >= 0; --i)
                {
                    if (j) {
                        update[i] = x_start_search;
                        iter = x_start_search->level[0].forward;
                    } else{
                        iter = x_start_search;
                    }

                    while (iter != x) {
                        if (iter == update[i]->level[i].forward) {
                            j = 1;
                            x_start_search = iter;
                            iter = iter->level[0].forward;
                            update[i] = update[i]->level[i].forward;
                            continue;
                        }

                        iter = iter->level[0].forward;
                    }
                }
                i = 0;
                break;
            }
            else {
                // not found the node
                ifree(update);
                return 1;
            }
        }
    }

    if (x->score != score) {
        // not found the node
        ifree(update);
        return 1;
    }

    for (i = 0; i <= list->level && update[i]->level[i].forward == x; i++) {
        update[i]->level[i].forward = x->level[i].forward;
        update[i]->level[i].span += x->level[i].span - 1;
    }
    for (; i <= list->level; i++) {
        --(update[i]->level[i].span);
    }
    skiplist_node_free(x);

    while (list->level > 0 && list->header->level[list->level].forward == NULL)
        list->level--;
    list->size--;

    ifree(update);
    return 0;
}

// ifree the skiplist
void skiplist_free(skiplist *list)
{
    snode *current_node = list->header->level[0].forward;
    while(current_node != NULL) {
        snode *next_node = current_node->level[0].forward;
        skiplist_node_free(current_node);
        current_node = next_node;
    }
    ifree(list);
}

// print the skip list, just for test.
static void skiplist_dump(skiplist *list)
{
    int *width = (int *)imalloc(sizeof(int) * (list->level + 1) * list->size);
    memset(width, 0, sizeof(int) * (list->level + 1) * list->size);
    snode **tempn = (snode **)imalloc(sizeof(snode*) * (list->level + 1));
    int i = 0, j;
    snode *x = list->header->level[0].forward;

    for (j = 0; j <= list->level; ++j) {
        tempn[j] = list->header->level[j].forward;
    }

    while (tempn[0] != NULL) {
        for (j = 1; j <= list->level; ++j) {
            if (tempn[j] == tempn[0]) {
                width[list->size * j + i] = tempn[j]->level[j].span;
                tempn[j] = tempn[j]->level[j].forward;
            } else {
                break;
            }
        }
        tempn[0] = tempn[0]->level[0].forward;
        ++i;
    }

    for (j = list->level; j > 0; --j) {
        for (i = 0; i < list->size; ++i) {
            if (width[j * list->size + i] == 0)
                printf("     ");
            else
                printf("%d    ", width[j * list->size + i]);
        }
        printf("\n");
    }
    while (x != NULL) {
        printf("%d:%d->", x->score, x->value);
        x = x->level[0].forward;
    }
    printf("NIL\n");

    ifree(width);
    ifree(tempn);
}

void test_skiplist(void) {
    time_t t;
    srand((unsigned)(time(&t)));

    int arr[][2] = { {3, 1}, {3,2}, {6,6}, {9,9}, {3, 3}, {1, 1}, {4, 4}, {8, 8}, {7, 7}, {5,5}}, i;
//    int arr[] = { 3, 6, 9}, i;
    skiplist_search_ret tempx;

    skiplist *list = skiplist_init();

    printf("search empty:--------------------\n");
    skiplist_search(list, 5, &tempx);
    if (tempx.index > 0) {
        printf("error, found not existed item!\n");
    }

    printf("delete empty:--------------------\n");
    skiplist_delete(list, 5, 2);

    printf("Insert:--------------------\n");
    for (i = 0; i < sizeof(arr) / sizeof(arr[0]); i++) {
        skiplist_insert(list, arr[i][0], arr[i][1]);
    }
    skiplist_dump(list);

    printf("search empty:--------------------\n");
    skiplist_search(list, 5, &tempx);
    printf("index = %d\n", tempx.index);

    printf("Search by index:-----------\n");
    int indexes[] = { 11, 3, 10 };

    for (i = 0; i < sizeof(indexes) / sizeof(indexes[0]); i++) {
        snode *tempnode = skiplist_at(list, indexes[i]);
        if (tempnode) {
            printf("index = %d, score = %d, value = %d\n", indexes[i], tempnode->score, tempnode->value);
        } else {
            printf("no index = %d\n", indexes[i]);
        }
    }

    printf("Delete:--------------------\n");
    skiplist_delete(list, 3, 2);
    skiplist_delete(list, 3, 1);
    skiplist_delete(list, 6, 6);
    skiplist_dump(list);

    clock_t start, finish;
    start = clock();
    for (i = 0; i < 30*1000; ++i) {
        if (rand() < RAND_MAX / 5 * 3) {
            skiplist_insert(list, RAND_UNIFORM(100), RAND_UNIFORM(20));
        }
        else {
            skiplist_delete(list, RAND_UNIFORM(100), RAND_UNIFORM(20));
        }
    }
    finish = clock();
    double duration = (double)(finish - start) / CLOCKS_PER_SEC;
    printf( "%f seconds\n", duration );

    printf("Search:--------------------\n");
    int keys[] = { 0, 3, 7, 100, 11 };

    for (i = 0; i < sizeof(keys) / sizeof(keys[0]); i++) {
        printf("index = %d, score = %d\n", skiplist_index_of_score(list, keys[i]), keys[i]);
    }

    skiplist_free(list);
};
