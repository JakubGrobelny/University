#include "csapp.h"

static pid_t spawn(void (*fn)(void)) {
  pid_t pid = Fork();
  if (pid == 0) {
    fn();
    printf("(%d) I'm done!\n", getpid());
    exit(EXIT_SUCCESS);
  }
  return pid;
}

static void grandchild(void) {
  printf("(%d) Waiting for signal!\n", getpid());
  /* TODO: Something is missing here! */
  printf("(%d) Got the signal!\n", getpid());
}

static void child(void) {
  pid_t pid;
  /* TODO: Something is missing here! */
  printf("(%d) Grandchild (%d) spawned!\n", getpid(), pid);
}

/* Runs command "ps -o pid,ppid,pgrp,stat,cmd" using execve(2). */ 
static void ps(void) {
  /* TODO: Something is missing here! */
}

int main(void) {
  /* set yourself a reaper */
  Prctl(PR_SET_CHILD_SUBREAPER, 1);
  printf("(%d) I'm a reaper now!\n", getpid());

  pid_t pid, pgrp;
  int status;

  /* TODO: Something is missing here! */

  return EXIT_SUCCESS;
}
