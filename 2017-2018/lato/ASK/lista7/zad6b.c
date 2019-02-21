#include <stdio.h>

extern int array[];

int test(int x)
{
    for (int i = 0; i < x; i++)
	printf("%d\n", array[i]);
    return 0;
}

