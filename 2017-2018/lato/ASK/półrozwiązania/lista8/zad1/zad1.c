#include <stdio.h>
#include <stdlib.h>

extern int clz(long);

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		fprintf(stderr, "Invalid number of arguments!\n");
		return 1;
	}

	long number = strtol(argv[1], NULL, 10);
	int result = clz(number);
	printf("%d\n", result);

	return 0;
}
