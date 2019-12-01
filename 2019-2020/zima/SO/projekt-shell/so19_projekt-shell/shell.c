#include <editline/readline.h>

#define DEBUG 0
#include "shell.h"

static int command(char **argv, bool bg) {
  int exitcode = 0;

  if ((exitcode = builtin_command(argv)) >= 0)
    return exitcode;

  sigset_t sigchld_mask, prev_mask;
  /* TODO: Block SIGCHLD temporarily. */

  pid_t pid = Fork();
  if (pid == 0) {
    /* TODO: Restore signal mask and put new process into separate group. */
    external_command(argv);
  }

  jobid_t jid = addjob(pid, bg);

  if (!bg) {
    while (true) {
      exitcode = jobdone(jid);
      /* TODO: If job is not done then wait for SIGCHLD, otherwise break. */
    }
  }

  /* TODO: Restore signal mask. */

  return exitcode;
}

static int eval(char *cmdline) {
  int argc;
  int exitcode = 0;
  token_t *argv = tokenize(cmdline, &argc);

  if (argc > 0) {
    bool bg = false;

    if (argc > 1 && argv[argc - 1] == T_BGJOB) {
      argv[--argc] = NULL;
      bg = true;
    }

    for (int i = 0; argv[i]; i++) {
      if (string_p(argv[i]))
        debug("argv[%d] = \"%s\"\n", i, argv[i]);
      else
        debug("argv[%d] = %ld\n", i, (long)argv[i]);
    }

    exitcode = command(argv, bg);
  }

  free(argv);

  return exitcode;
}

int main(int argc, char *argv[]) {
  rl_readline_name = "ii-sh";
  rl_catch_signals = 1;
  rl_initialize();

  initjobs();

  char *line;
  while (true) {
    __unused int exitcode = 0;
    line = readline("# ");

    if (line == NULL)
      break;

    add_history(line);
    exitcode = eval(line);
    debug("exitcode = %d\n", WEXITSTATUS(exitcode));
    free(line);
    watchjobs();
  }

  msg("\n");
  killjob(JOBID_EVERY, SIGINT);

  return 0;
}
