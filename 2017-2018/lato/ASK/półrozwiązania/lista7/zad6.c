extern int array[];
int test();

int fun() {
    array[0] = 1;
    array[1] = 2;
    array[2] = 3;
}


int main()
{
    fun();
    test(1101);
    return 0;
}
