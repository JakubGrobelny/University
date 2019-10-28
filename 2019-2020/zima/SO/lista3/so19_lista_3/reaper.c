#include "csapp.h"


static pid_t spawn(void (*fn)(void)) 
{
    pid_t pid = Fork();
    if (pid == 0) 
    {
        fn();
        printf("(%d) I'm done!\n", getpid());
        exit(EXIT_SUCCESS);
    }
    return pid;
}


static void handler(int sig)
{
    return;
}

static void grandchild(void) 
{
    printf("(%d) Waiting for signal!\n", getpid());
    signal(SIGINT, handler);
    pause();
    printf("(%d) Got the signal!\n", getpid());
}


static void child(void) 
{
    Setpgid(getpid(), getpid());
    pid_t pid = spawn(grandchild);
    printf("(%d) Grandchild (%d) spawned!\n", getpid(), pid);
}


/* Runs command "ps -o pid,ppid,pgrp,stat,cmd" using execve(2). */ 
static void ps(void) 
{
    char* const argv[] = {
        "/usr/bin/ps", "-o", "pid,ppid,pgrp,stat,cmd", NULL
    };

    execve(argv[0], argv, NULL);
}


int main(void) 
{
    /* set yourself a reaper */
    Prctl(PR_SET_CHILD_SUBREAPER, 1);
    printf("(%d) I'm a reaper now!\n", getpid());

    pid_t pgrp;
    int status;

    pgrp = spawn(child);
    Waitpid(pgrp, NULL, 0);

    pid_t ps_pid = spawn(ps);
    Waitpid(ps_pid, NULL, 0);

    Kill(-pgrp, SIGINT);

    printf("(%d) Sending SIGINT to group %d!\n", getpid(), pgrp);

    Waitpid(-1, &status, 0);

    printf("(%d) Grandchild exit code: %d!\n", getpid(), WEXITSTATUS(status));

    return EXIT_SUCCESS;
}
