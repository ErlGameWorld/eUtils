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
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hqueue.h"


struct hqueue
{
    int version;
    uint32_t idx;
    uint32_t max_elems;
    uint32_t heap_size;
    hqnode_t* heap; // one based index
};


struct hqnode
{
    double priority;
    void* value;
};


static inline void
hqueue_exchange(hqueue_t* hqueue, int i, int j)
{
    hqnode_t tmp;

    tmp = hqueue->heap[i];
    hqueue->heap[i] = hqueue->heap[j];
    hqueue->heap[j] = tmp;
    return;
}


static inline int
hqueue_less(hqueue_t* hqueue, int i, int j)
{
    return hqueue->heap[i].priority < hqueue->heap[j].priority;
}


static void
hqueue_fix_up(hqueue_t* hqueue, int k)
{
    while(k > 1 && hqueue_less(hqueue, k/2, k)) {
        hqueue_exchange(hqueue, k/2, k);
        k = k/2;
    }
    return;
}


static void
hqueue_fix_down(hqueue_t* hqueue, int k)
{
    int j;
    int n = hqueue->idx;

    while(2*k <= n) {
        j = 2*k;
        if(j < n && hqueue_less(hqueue, j, j+1)) {
            j++;
        }
        if(!hqueue_less(hqueue, k, j)) {
            break;
        }
        hqueue_exchange(hqueue, k, j);
        k = j;
    }
    return;
}


hqueue_t*
hqueue_new(uint32_t max_elems, uint32_t heap_size)
{
    hqueue_t* hqueue = NULL;
    size_t total_heap_size;

    if(max_elems == 0 || heap_size == 0) {
        return NULL;
    }

    if(max_elems < heap_size) {
        heap_size = max_elems;
    }

    hqueue = HQUEUE_ALLOC(sizeof(hqueue_t));
    if(hqueue == NULL) {
        return NULL;
    }

    memset(hqueue, '\0', sizeof(hqueue_t));
    hqueue->version = HQ_VERSION;
    hqueue->max_elems = max_elems;
    hqueue->heap_size = heap_size;
    hqueue->idx = 0;

    total_heap_size = sizeof(hqnode_t) * (hqueue->heap_size+1);

    hqueue->heap = (hqnode_t*) HQUEUE_ALLOC(total_heap_size);

    if(hqueue->heap == NULL ) {
        HQUEUE_FREE(hqueue);
        return NULL;
    }

    memset(hqueue->heap, '\0', total_heap_size);

    return hqueue;
}


void
hqueue_free(hqueue_t* hqueue)
{
    HQUEUE_FREE(hqueue->heap);
    HQUEUE_FREE(hqueue);

    return;
}


void
hqueue_free2(hqueue_t* hqueue, void (*free_node)(void* node))
{
    uint32_t i;

    for(i = 1; i < hqueue->heap_size + 1; i++) {
        if(i <= hqueue->idx) {
            free_node(hqueue->heap[i].value);
        } else {
            assert(hqueue->heap[i].value == NULL && "inactive elements must be NULL");
        }
    }

    hqueue_free(hqueue);

    return;
}


// Extraction order is undefined for entries with duplicate priorities
int
hqueue_extract_max(hqueue_t* hqueue, double* priority, void** value)
{
    if(hqueue->idx <= 0) {
        return 0;
    }

    hqueue_exchange(hqueue, 1, hqueue->idx);

    *priority = hqueue->heap[hqueue->idx].priority;
    *value = hqueue->heap[hqueue->idx].value;

    hqueue->heap[hqueue->idx].value = NULL;

    hqueue->idx--; // heap uses one based index, so we decrement after
    hqueue_fix_down(hqueue, 1);

    return 1;
}


void
hqueue_get_elem(hqueue_t* hqueue, uint32_t idx, double *priority, void** value)
{
    *priority = hqueue->heap[idx].priority;
    *value = hqueue->heap[idx].value;

    return;
}


static int
hqueue_maybe_resize(hqueue_t* hqueue)
{
    uint32_t min_resize;

    if(hqueue->idx + 1 > hqueue->heap_size) {
        if(hqueue->idx * HQ_SCALE_FACTOR > hqueue->max_elems) {
            min_resize = hqueue->max_elems;
        } else {
            min_resize = hqueue->idx * HQ_SCALE_FACTOR;
        }
        return hqueue_resize_heap(hqueue, min_resize);
    }

    return 1;
}


int
hqueue_insert(hqueue_t* hqueue, double priority, void* value)
{
    if(hqueue->idx >= hqueue->max_elems) {
        return 0;
    }

    if(!hqueue_maybe_resize(hqueue)) {
        return 0;
    }

    hqueue->idx++; // heap uses one based index, so we increment first
    hqueue->heap[hqueue->idx].priority = priority;
    hqueue->heap[hqueue->idx].value = value;

    hqueue_fix_up(hqueue, hqueue->idx);

    return 1;
}


uint32_t
hqueue_size(hqueue_t* hqueue)
{
    return hqueue->idx;
}


uint32_t
hqueue_heap_size(hqueue_t* hqueue)
{
    return hqueue->heap_size;
}


uint32_t
hqueue_max_elems(hqueue_t* hqueue)
{
    return hqueue->max_elems;
}


void
hqueue_scale_by(hqueue_t* hqueue, double factor)
{
    uint32_t i;

    for(i = 1; i <= hqueue->idx && i <= hqueue->heap_size; i++) {
        hqueue->heap[i].priority *= factor;
    }

    return;
}


uint32_t
hqueue_resize_heap(hqueue_t* hqueue, uint32_t new_heap_size)
{
    uint32_t old_heap_size;
    size_t total_heap_size;
    hqnode_t* tmp_heap;
    uint32_t i;

    if(hqueue->idx > new_heap_size) {
        return 0;
    }

    total_heap_size = sizeof(hqnode_t) * (new_heap_size+1);
    old_heap_size = hqueue->heap_size;

    if((tmp_heap = (hqnode_t*) HQUEUE_ALLOC(total_heap_size)) == NULL) {
        return 0;
    }

    memset(tmp_heap, '\0', total_heap_size);

    for(i = 1; i <= hqueue->idx && i <= old_heap_size; i++) {
        if(i <= hqueue->idx) {
            tmp_heap[i] = hqueue->heap[i];
            hqueue->heap[i].value = NULL;
        } else {
            assert(hqueue->heap[i].value == NULL &&
                "unexpected NULL element during heap resize");
        }
    }

    HQUEUE_FREE(hqueue->heap);
    hqueue->heap = tmp_heap;
    hqueue->heap_size = new_heap_size;

    return old_heap_size;
}


int
hqueue_set_max_elems(hqueue_t* hqueue, uint32_t new_max_elems)
{
    uint32_t old_max_elems;

    if(hqueue->heap_size > new_max_elems) {
        if(!hqueue_resize_heap(hqueue, new_max_elems)) {
            return 0;
        }
    }

    old_max_elems = hqueue->max_elems;
    hqueue->max_elems = new_max_elems;

    return old_max_elems;
}
