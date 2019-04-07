#include <stdio.h>
#include <stdlib.h>

void* malloc_pattern(size_t size, const void* pattern, size_t pattern_len)
{
    char* pointer = (char*)malloc(size);

    for (int i = 0; i < size; i++)
    {
        pointer[i] = ((char*)pattern)[i%pattern_len];
    }

    return pointer;
}

void* realloc_pattern(void* t, size_t old_size, size_t new_size, const void* pattern, size_t pattern_len)
{
    t = realloc(t, new_size);


    for (int i = old_size; i < new_size; i++)
    {
        ((char*)(t))[i] = ((char*)(pattern))[i%pattern_len];
    }

    return t;
}

int main()
{
    const char *p1 = "abc";
    char* t = (char*)malloc_pattern(5, p1, 3);
    const char *p2 = "cde";
    t = (char*)realloc_pattern(t, 5, 10, p2, 3);
    printf("%.10s\n", t);

    return 0;
}
