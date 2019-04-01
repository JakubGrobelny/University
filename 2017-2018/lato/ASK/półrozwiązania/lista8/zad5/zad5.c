#include <stdio.h>
#include <stdlib.h>

unsigned mulf(unsigned a, unsigned b);

int main(int argc, char* argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "Invalid number of arguments!\n");
		return 1;
	}

	unsigned a = (unsigned)strtoul(argv[1], NULL, 10);
	unsigned b = (unsigned)strtoul(argv[2], NULL, 10);

	unsigned result = mulf(a, b);
	float f = *((float*)&result);

	printf("%f\n", f);
	printf("%x\n", result);
	return 0;
}
