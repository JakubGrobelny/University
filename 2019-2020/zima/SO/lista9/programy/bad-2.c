#include "../../lib/include/csapp.h"

int main(int argc, char *argv[]) {
  if (argc < 2)
    app_error("Give me a string!");
  const char *s = argv[1];
  char *p = malloc(strlen(s));
  strcpy(p, s);
  free(p);
  return 0;
}
