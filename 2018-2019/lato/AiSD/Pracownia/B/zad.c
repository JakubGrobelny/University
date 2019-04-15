#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define max_cap 1400000 // ~11.2 MB

uint64_t array[max_cap];
size_t printed = 0;
uint64_t M;
uint64_t k;
uint64_t last_printed = 0;

struct heap
{
    uint64_t* elements;
    size_t size;
    size_t capacity;
};


size_t heap_init(struct heap* h)
{
    h->capacity = max_cap;
    h->size = 0;

    h->elements = array;
    // h->elements = malloc(sizeof(uint64_t) * h->capacity);

    return (size_t)h->elements;
}


uint64_t heap_find_max(struct heap* h)
{
    // indeksowanie od 1 żeby łatwiej było wyszukiwać synów
    return h->elements[1];
}


void heap_move_down(struct heap* h, size_t index)
{
    size_t father = index;
    size_t child;

    do
    {
        child = father;
        size_t left = 2 * father;
        size_t right = left + 1;

        if ((left <= h->size) && (h->elements[left] > h->elements[father]))
            father = left;
        if ((left < h->size) && (h->elements[right] > h->elements[father]))
            father = right;
        
        size_t temp = h->elements[child];
        h->elements[child] = h->elements[father];
        h->elements[father] = temp;
    }
    while (child != father);
}

void heap_move_up(struct heap* h, size_t index)
{
    size_t child = index;
    size_t father;

    do
    {
        father = child;
        if ((father > 1) && (h->elements[father >> 1] < h->elements[child]))
            child = father >> 1;

        size_t temp = h->elements[child];
        h->elements[child] = h->elements[father];
        h->elements[father] = temp;

    }
    while (father != child);
}


void heap_remove_max(struct heap* h)
{
    h->elements[1] = h->elements[h->size--];
    heap_move_down(h, 1);
}


void heap_push(struct heap* h, uint64_t element)
{
    if (h->size < max_cap - 1)
    {
        h->size++;
        h->elements[h->size] = element;
        heap_move_up(h, h->size);
    }
    else if (h->elements[h->size] < element)
    {
        h->elements[h->size] = element;
        heap_move_up(h, h->size);
    }
}


void heap_remove_biggest(struct heap* h, uint64_t bound)
{
    if (!h->size)
        return;

    uint64_t max = heap_find_max(h);

    while(max > bound && h->size)
    {
        if (printed == k)
            return;

        if (max != last_printed)
        {
            printf("%lu\n", max);
            printed++;
            last_printed = max;
        }

        heap_remove_max(h);
        max = heap_find_max(h);
    }
}


void heap_add_column(struct heap* h, int64_t column)
{
    heap_remove_biggest(h, M * column);

    if (printed == k)
        return;

    for (int64_t row = (int64_t)M; row >= column; row--)
        heap_push(h, row * column);
}


void solve(struct heap* h)
{
    for (int64_t column = (int64_t)M; column > 0; column--)
    {
        heap_add_column(h, column);

        if (printed == k)
            return;
    }

    if (printed != k)
    {
        heap_remove_biggest(h, 0);
    }
}


int main()
{
    scanf("%ld %ld", &M, &k);

    struct heap h;
    heap_init(&h);
    solve(&h);
    return 0;
}