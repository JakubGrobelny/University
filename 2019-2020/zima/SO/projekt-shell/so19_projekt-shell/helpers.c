#include "shell.h"

void strapp(char **dstp, const char *src) {
  assert(dstp != NULL);

  if (*dstp == NULL) {
    *dstp = strdup(src);
  } else {
    size_t s = strlen(*dstp) + strlen(src) + 1;
    *dstp = realloc(*dstp, s);
    strcat(*dstp, src);
  }
}

#define tokcmp(t) (strncmp(tok, (t), sizeof(t)) == 0)

token_t *tokenize(char *s, int *tokc_p) {
  int capacity = 10;
  int n = 0;
  token_t *tokv = malloc(sizeof(token_t) * (capacity + 1));

  while (*s != '\0') {
    while (isspace(*s) && *s != '\0')
      s++;

    if (n == capacity) {
      capacity *= 2;
      tokv = realloc(tokv, sizeof(token_t) * (capacity + 1));
    }
    token_t tok = s;

    while (!isspace(*s) && *s != '\0')
      s++;

    if (tokcmp("&&"))
      tok = T_AND;
    else if (tokcmp("||"))
      tok = T_OR;
    else if (tokcmp("|"))
      tok = T_PIPE;
    else if (tokcmp(">"))
      tok = T_OUTPUT;
    else if (tokcmp("<"))
      tok = T_INPUT;
    else if (tokcmp("&"))
      tok = T_BGJOB;
    else if (tokcmp(";"))
      tok = T_COLON;
    else if (tokcmp("!"))
      tok = T_NOT;

    tokv[n++] = tok;
    *s++ = '\0';
  }

  tokv[n] = NULL;
  *tokc_p = n;
  return tokv;
}
