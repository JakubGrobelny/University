#include <stdio.h>
#include <stdlib.h>

#define MAX_CHARACTERS 256

typedef struct Node
{
    struct Node* l_child;
    struct Node* r_child;

    char character;
    unsigned int occurrences;

    unsigned int bit_size;

} Node;

Node create_node(char character, unsigned int occurrences)
{
    Node new;

    new.l_child = NULL;
    new.r_child = NULL;

    new.character = character;
    new.occurrences = occurrences;

    new.bit_size = 0;

    return new;
}

Node create_root(Node* l_child, Node* r_child)
{
    Node new;

    new.l_child = l_child;
    new.r_child = r_child;
    new.bit_size = 0;

    new.occurrences = l_child->occurrences + r_child->occurrences;
    new.character = '\0';

    return new;
}

typedef struct
{
    unsigned int first;
    unsigned int size;
    unsigned int capacity;
    Node* list;

} List;

void init_list(List* list, int initial_capacity)
{
    list->first = 0;
    list->size = 0;
    list->capacity = initial_capacity;
    list->list = (Node*)malloc(sizeof(Node) * initial_capacity);
}

void push_back(List* list, Node node)
{
    if (list->size >= list->capacity)
    {
        list->capacity *= 2;
        list->list = (Node*)realloc(list->list, sizeof(Node) * list->capacity);

        for (int i = 0; i < list->first; i++)
        {
            list->list[list->first + list->size + i] = list->list[i];
        }
    }

    list->list[(list->first + list->size) % list->capacity] = node;
    list->size++;
}

Node* get_element(List* list, int position)
{
    return &list->list[(list->first + position) % list->capacity];
}

void set_element(List* list, int position, Node node)
{
    list->list[(list->first + position) % list->capacity] = node;
}

Node pop_inside(List* list, int position)
{
    Node node = list->list[(list->first + position) % list->capacity];

    for (int i = position; i < list->size - 1; i++)
    {
        set_element(list, i, *get_element(list, i + 1));
    }

    list->size--;
    return node;
}

void read_text(unsigned int* occurrences)
{
    unsigned int total = 0;

    char c;
    while ((c = getchar()) != EOF)
    {
        total++;
        occurrences[(int)c]++;
    }
}

void fill_list(List* list, unsigned int* occurrences)
{
    for (int i = 0; i < MAX_CHARACTERS; i++)
    {
        if (occurrences[i])
        {
            push_back(list, create_node((char)i, occurrences[i]));
        }
    }
}

int least_occurrences(List* list)
{
    double min = get_element(list, 0)->occurrences;
    int min_index = 0;

    for (int i = 0; i < list->size; i++)
    {
        if (get_element(list, i)->occurrences < min)
        {
            min_index = i;
            min = get_element(list, i)->occurrences;
        }
    }

    return min_index;
}

void create_tree(List* list, List* nodes) // ZALOZMY ZE ZDIALA iksde
{
    while (list->size > 1)
    {
        Node first = pop_inside(list, least_occurrences(list));
        Node second = pop_inside(list, least_occurrences(list));

        push_back(nodes, first);
        push_back(nodes, second);

        push_back(list, create_root(get_element(nodes, nodes->size - 1), get_element(nodes, nodes->size - 2)));
    }
}

void assign_code(Node* tree, int length)
{
    if (tree->character != '\0')
    {
        tree->bit_size = length;
    }
    else
    {
        if (tree->l_child != NULL)
            assign_code(tree->l_child, length + 1);
        if (tree->r_child != NULL)
            assign_code(tree->r_child, length + 1);
    }
}

unsigned int total_size(Node* tree)
{
    if (tree->character != '\0')
    {
        return (tree->occurrences * tree->bit_size);
    }
    else
    {
        return total_size(tree->l_child) + total_size(tree->r_child);
    }
}

//TEST

void print_list(List* list)
{
    for (int i = 0; i < list->size; i++)
    {
        printf("%c %d\n", get_element(list, i)->character, get_element(list, i)->occurrences);
    }
}

void print_tree(Node* tree)
{
    if (tree->character != '\0')
    {
        printf(" %c - %d - %d", tree->character, tree->occurrences, tree->bit_size);
    }
    else
    {
        if (tree->l_child != NULL)
            print_tree(tree->r_child);
        if (tree->r_child != NULL)
            print_tree(tree->l_child);
        printf("\n");
    }
}
// TEST

int main()
{
    unsigned int occurrences[MAX_CHARACTERS] = {0.0f};

    read_text(occurrences);

    List trees_list;
    init_list(&trees_list, 5);
    fill_list(&trees_list, occurrences);

    List nodes;
    init_list(&nodes, 5);

    create_tree(&trees_list, &nodes);
    Node* tree = get_element(&trees_list, 0);

    assign_code(tree, 0);
    printf("%d", total_size(tree));

    return 0;
}

