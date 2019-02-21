#include <stdio.h>
#include <stdint.h>

struct A
{
    int32_t b;
    int16_t d;
    int8_t a;
    int8_t c;
};

struct B
{
    int64_t b;
    int32_t c;
    int16_t a;
};

int main()
{
    struct A struct_a;
    struct B struct_b;


    printf("sizeof(A) = %d \nsizoef(a) = %d \nsizeof(b) = %d \nsizeof(c) = %d \nsizeof(d) = %d\n", sizeof(struct_a), sizeof(struct_a.a), sizeof(struct_a.b), sizeof(struct_a.c), sizeof(struct_a.d));        

    printf("sizeof(B) = %d \nsizeof(a) = %d \nsizeof(b) = %d \nsizeof(c) = %d\n", sizeof(struct_b), sizeof(struct_b.a), sizeof(struct_b.b), sizeof(struct_b.c));

    return 0;
}
