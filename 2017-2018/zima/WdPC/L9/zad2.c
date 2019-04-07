#include <stdio.h>
#include <stdlib.h>

typedef struct Element
{
    struct Element* parent;
    int identifier;

} Element;

Element* tree;
int tree_size = 1;
int tree_capacity = 1;

Element* find_element(int identifier)
{
    for (int i = 0; i < tree_size; i++)
    {
        if (tree[i].identifier == identifier)
            return &tree[i];
    }

    return NULL;
}

void add_element(int identifier, int parent_identifier)
{
    if (tree_size >= tree_capacity) // allocating more memory in the array
    {
        tree_capacity *= 2;
        tree = realloc(tree, sizeof(Element) * tree_capacity);
    }

    tree[tree_size].parent = find_element(parent_identifier);
    tree[tree_size].identifier = identifier;

    tree_size++;
}

void move_subtree(int identifier, int new_parent_identifier)
{
    Element* new_parent = find_element(new_parent_identifier);
    Element* subtree = find_element(identifier);
    subtree->parent = new_parent;
}

int find_depth()
{
    Element* element = find_element(2);

    int depth = 1;

    while (element->parent != NULL)
    {
        depth++;
        element = element->parent;
    }

    return depth;
}

int main()
{
    tree = malloc(sizeof(Element) * tree_capacity);
    tree[0].parent = NULL;
    tree[0].identifier = 1;

    char operation;
    int new_identifier;
    int parent_identifier;

    int n;
    scanf("%d", &n);

    for (int i = 0; i < n; i++)
    {
        scanf(" %c %d %d", &operation, &new_identifier, &parent_identifier);

        if (operation == 'd')
            add_element(new_identifier, parent_identifier);
        else if (operation == 'p')
            move_subtree(new_identifier, parent_identifier);
    }

    printf("%d", find_depth());

    return 0;
}

