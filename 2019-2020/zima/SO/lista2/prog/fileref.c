#include "csapp.h"

static char buf[256];

#define LINE1 49
#define LINE2 33
#define LINE3 78

static void do_read(int fd) 
{
    pid_t fork_pid = Fork();
    pid_t pid = getpid();

    const int chars_to_read = 16;

    const off_t prev_offset = Lseek(fd, 0, SEEK_CUR);
    printf("[%d]: before %ld\n", pid, prev_offset);

    if (Read(fd, buf, chars_to_read) != chars_to_read) 
        app_error("Failed to read the file!");

    const off_t end_offset = Lseek(fd, 0, SEEK_CUR);
    printf("[%d]: after %ld\n", pid, end_offset);

    if (fork_pid) 
    {
        Waitpid(-1, NULL, WUNTRACED);
        Close(fd);
    }

    exit(0);
}

static void do_close(int fd) 
{
    if (Fork()) 
    {
        Close(fd);
        printf("[%d]: closed the file.\n", getpid());
        Waitpid(-1, NULL, WUNTRACED);
        exit(0);
    }
    else
    {
        pid_t pid = getpid();

        sleep(1);

        if (Read(fd, buf, LINE1) != LINE1)
            app_error("[%d]: Failed to read from file!", pid);
        
        buf[LINE1] = '\0';
        printf("[%d]: %s", pid, buf);

        exit(0);
    }
}

int main(int argc, char **argv) 
{
    if (argc != 2)
        app_error("Usage: %s [read|close]", argv[0]);

    int fd = Open("test.txt", O_RDONLY, 0);

    if (!strcmp(argv[1], "read"))
        do_read(fd);
    if (!strcmp(argv[1], "close"))
        do_close(fd);

    app_error("Unknown variant '%s'", argv[1]);
}
