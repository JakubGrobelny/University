#include <stdio.h>
#include <stdint.h>


int main(int argc, char* argv[]) {
    for (uint64_t i = 0; i < 5000000; i++) {
	printf("%s\n", argv[0]);
    }

    return 0;
}
