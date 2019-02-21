#include <stdio.h>
#include <stdlib.h>

void insert_sort(long* first, long* last);

void print_array(long* first, long* last)
{
	while (first != last)
	{
		printf("%ld ", *first);
		first += 1;
	}
	putchar('\n');
}

int main(int argc, char* argv[])
{
	if (argc < 2)
	{
		fprintf(stderr, "Invalid number of arguments!\n");
		return 1;
	}

	long* array = malloc(sizeof(long) * (argc - 1));

	for (int i = 0; i < argc - 1; i++)
		array[i] = strtol(argv[i + 1], NULL, 10);

	insert_sort(array, array + (argc - 1));
	print_array(array, array + (argc - 1));
	return 0;
}	
