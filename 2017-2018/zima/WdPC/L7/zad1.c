#include <stdio.h>
#include <stdlib.h>

struct ArrayQueue
{
    int *t;
    size_t size; // liczba elementów
    size_t first; // indeks pierwszego elementu w kolejce
    size_t capacity; // pojemnosc kolejki
};

int index(struct ArrayQueue* queue, int i)
{
    return ((queue->first + i) % queue->capacity);
}

struct ArrayQueue make_queue(size_t initial_capacity)
{
    struct ArrayQueue queue;
    queue.t = (int*)malloc(initial_capacity*sizeof(int));
    queue.capacity = initial_capacity;
    queue.size = 0;
    queue.first = 0;

    return queue;
}

void push_last(struct ArrayQueue *q, int value)
{
    if (q->size >= q->capacity) // jeżeli nie ma miejsca na kolejny element
    {
        q->t = realloc(q->t, q->capacity * 2 * sizeof(int));

        for (int i = 0; i < q->first; i++)
        {
            q->t[q->capacity + i] = q->t[i];
        }

        q->capacity = q->capacity * 2;

    }

    q->t[index(q, q->size)] = value;
    q->size++;
}

int pop_first(struct ArrayQueue *q)
{
    int first_value = q->t[q->first];
    q->first++;

    if (q->first >= q->capacity)
        q->first = 0;

    q->size--;
    return first_value;
}

int main()
{

    struct ArrayQueue q = make_queue(1);

    for (int i = 0; i < 4; i++)
        push_last(&q, i);

    printf("%lu %lu %lu,", q.size, q.first, q.capacity);


    for (int i = 0; i < 7; i++)
    {
        printf(" %i", pop_first(&q));
        push_last(&q, i);
    }


    push_last(&q, 0);
    printf(", %i, %lu %lu %lu\n", pop_first(&q), q.size, q.first, q.capacity);

    free(q.t);
    return 0;
}

