#ifndef _SHELL_H_
#define _SHELL_H_

#include "csapp.h"

#define msg(...) dprintf(STDERR_FILENO, __VA_ARGS__)

#if DEBUG > 0
#define debug(...) dprintf(STDERR_FILENO, __VA_ARGS__)
#else
#define debug(...)
#endif

typedef char *token_t;

#define T_NULL ((token_t)0)
#define T_AND ((token_t)1)
#define T_OR ((token_t)2)
#define T_PIPE ((token_t)3)
#define T_BGJOB ((token_t)4)
#define T_COLON ((token_t)5)
#define T_OUTPUT ((token_t)6)
#define T_INPUT ((token_t)7)
#define T_APPEND ((token_t)8)
#define T_NOT ((token_t)9)
#define separator_p(t) ((t) <= T_COLON)
#define string_p(t) ((t) > T_NOT)

void strapp(char **dstp, const char *src);
token_t *tokenize(char *s, int *tokc_p);

typedef unsigned jobid_t;

#define JOBID_FG ((jobid_t)0) /* slot for foreground job */
#define JOBID_BG ((jobid_t)1) /* first slot for background job */
#define JOBID_EVERY ((jobid_t)-1) /* perform an action for all jobs */

void initjobs(void);
jobid_t addjob(pid_t pid, bool bg);
int jobdone(jobid_t jid);
void watchjobs(void);
void killjob(jobid_t jid, int sig);

int builtin_command(char **argv);
noreturn void external_command(char **argv);

#endif /* !_SHELL_H_ */
