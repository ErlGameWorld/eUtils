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

#pragma once


#include <stdint.h>

#define HQ_VERSION 0
#define HQ_SCALE_FACTOR 2 // heap expansion scale factor


// Override the default memory allocator to use the Erlang versions.
// This bubbles up memory usage for the NIF into Erlang stats.
#ifdef HQ_ENIF_ALLOC

#include "erl_nif.h"

#define HQUEUE_ALLOC enif_alloc
#define HQUEUE_FREE enif_free

#else

#define HQUEUE_ALLOC malloc
#define HQUEUE_FREE free

#endif


typedef struct hqnode hqnode_t;
typedef struct hqueue hqueue_t;


hqueue_t* hqueue_new(uint32_t max_elems, uint32_t heap_size);

void hqueue_free(hqueue_t* hqueue);
void hqueue_free2(hqueue_t* hqueue, void (*free_node)(void* node));

int hqueue_insert(hqueue_t* hqueue, double priority, void* val);
int hqueue_extract_max(hqueue_t* hqueue, double* priority, void** value);
void hqueue_get_elem(hqueue_t* hqueue, uint32_t idx, double *priority,
        void** value);

uint32_t hqueue_size(hqueue_t* hqueue);
uint32_t hqueue_heap_size(hqueue_t* hqueue);

uint32_t hqueue_max_elems(hqueue_t* hqueue);
int hqueue_set_max_elems(hqueue_t* hqueue, uint32_t new_max_elems);

void hqueue_scale_by(hqueue_t* hqueue, double factor);
uint32_t hqueue_resize_heap(hqueue_t* hqueue, uint32_t new_heap_size);
