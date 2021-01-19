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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "hqueue.h"


// Simple test script to stress the public HQueue API.
// Primary use case is for running this under Valgrind.
int main(void)
{
    int str_len = 100;
    int iterations = 1000;
    uint32_t max_elems = 1024;
    uint32_t heap_size = 64;
    hqueue_t* hq = hqueue_new(max_elems, heap_size);
    double priority;
    double priority_res;
    char* val;
    char* val_res;
    int i;

    assert(max_elems == hqueue_max_elems(hq));
    assert(heap_size == hqueue_heap_size(hq));

    for(i = 0; i < iterations; i++) {
        priority = 1234.4321 * i;
        val = (char*) malloc(str_len + 1);

        if(val == NULL) {
            return 1;
        }

        assert(hqueue_size(hq) == i);

        if(snprintf(val, str_len + 1, "Fun string #%d\n", i)) {
            if(!hqueue_insert(hq, priority, val)) {
                return 1;
            }
        } else {
            return 1;
        }
    }

    hqueue_scale_by(hq, 3.7);

    // Added 1000 elements, so heap size should have expanded to 1024
    assert(max_elems == hqueue_max_elems(hq));
    assert(max_elems == hqueue_heap_size(hq));

    if(!hqueue_extract_max(hq, &priority_res, (void**) &val_res)) {
        return 1;
    }
    free(val_res);

    hqueue_free2(hq, free);

    return 0;
}

