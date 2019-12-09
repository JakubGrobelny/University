#include "../../lib/include/csapp.h"
#include "../../lib/include/queue.h"

typedef struct node {
  SLIST_ENTRY(node) link;
  long value;
} node_t;

typedef SLIST_HEAD(, node) list_t;

node_t *new_node(long value) {
  node_t *node = malloc(sizeof(node_t));
  node->value = value;
  return node;
}

#define N 10

int main(void) {
  list_t head;
  for (int i = 0; i < N; i++)
    SLIST_INSERT_HEAD(&head, new_node(i), link);
  free(SLIST_FIRST(&head));
  return 0;
}
