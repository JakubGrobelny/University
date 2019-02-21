#include <stdio.h>
#include <stdlib.h>

unsigned long fibonacci(unsigned long n);

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		fprintf(stderr, "Invalid number of arguments!\n");
		return 1;
	}

	unsigned long n = strtoul(argv[1], NULL, 10);
	unsigned long fib = fibonacci(n);
	printf("%lu\n", fib);
	return 0;
} 
