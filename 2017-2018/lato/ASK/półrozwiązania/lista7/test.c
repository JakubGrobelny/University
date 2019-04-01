int a = 2;
int b;

static int c = 4;
static int d;

static void fun() {
    static int e = 6;
    static int f;
}

int main() {
    fun();
}
