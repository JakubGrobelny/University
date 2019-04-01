#include <stdio.h>
#include <stdlib.h>

typedef struct
{
	unsigned long lcm;
	unsigned long gcd;
} result_t;

result_t lcm_gcd(unsigned long a, unsigned long b);

int main(int argc, char* argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "Invalid input!\n");
		return 1;
	}

	unsigned long a = strtoul(argv[1], NULL, 10);
	unsigned long b = strtoul(argv[2], NULL, 10);

	result_t result = lcm_gcd(a, b);

	printf("LCM = %lu, GCD = %lu\n", result.lcm, result.gcd);

	return 0;	
}
