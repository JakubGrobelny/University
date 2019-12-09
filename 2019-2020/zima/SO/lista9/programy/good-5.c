#include "../../lib/include/csapp.h"
#include "../../lib/include/queue.h"

typedef struct node {
  SLIST_ENTRY(node) link;
  long value;
} node_t;

typedef SLIST_HEAD(, node) list_t;

node_t* new_node(long value) {
  node_t* node = malloc(sizeof(node_t));
  node->value = value;
  return node;
}

void free_list(node_t* list) {
  if (list) {
    free_list(SLIST_NEXT(list, link));
    free(list);
  }
}

#define N 10

int main(void) {
  list_t head = {.slh_first = NULL};
  for (int i = 0; i < N; i++) {
    node_t* node = new_node(i);
    SLIST_INSERT_HEAD(&head, node, link);
  }

  free_list(SLIST_FIRST(&head));
  return 0;
}
