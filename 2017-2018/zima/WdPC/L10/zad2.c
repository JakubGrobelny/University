#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>


typedef struct Stack
{
    char* elements;
    unsigned int size;
    unsigned int capacity;

} Stack;

Stack init_stack()
{
    Stack new;
    new.capacity = 1;
    new.size = 0;
    new.elements = malloc(sizeof(char) * new.capacity);

    return new;
}

char pop(Stack* stack)
{
    stack->size--;
    return stack->elements[stack->size];
}

void push(Stack* stack, char element)
{
    if (stack->capacity <= stack->size)
    {
        stack->capacity *= 10;
        stack->elements = realloc(stack->elements, sizeof(char) * stack->capacity);
    }
    stack->elements[stack->size] = element;
    stack->size++;
}


void print_stack(Stack* stack)
{
    for (int i = 0; i < stack->size; i++)
    {
        putchar(stack->elements[i]);
    }
    putchar('\n');
}

void offset_stack(Stack* stack, int offset, int start)
{
    for (int i = start; i < stack->size - offset; i++)
    {
        stack->elements[i] = stack->elements[i + offset];
    }

    stack->size -= offset;
}

void remove_double_negations(char* formula, int* length)
{
    // TODO: program usuwa negacje pojedynczo zamiast parami
    for (int i = 0; i < (*length)-1; i++)
    {
        if (formula[i] == '~' && formula[i+1] == '~')
        {
            for (int e = i; e < *length - 2; e++)
            {
                formula[e] = formula[e+2];
            }
            i--;
            (*length)-=2;
        }

    }
}

/*
void remove_double_negations(Stack* stack)
{
    for (int i = 1; i < stack->size; i++)
    {
        if (stack->elements[i] == '~')
        {
            if (stack->elements[i-1] == '~')
            {
                offset_stack(stack, 1, i);
            }
        }
    }
}
*/

Stack make_RPN(char* formula, int length)
{
    Stack stack = init_stack();
    Stack output = init_stack();
    remove_double_negations(formula, &length);

    //print formula
    //for (int i = 0; i < length; i++)
    //{
//        printf("%c", formula[i]);
    //}
    //printf("\n");

    for (int i = 0; i < length; i++)
    {
        //printf("element: %c\n", formula[i]);
        //printf("stack: ");
        //print_stack(&stack);
        //printf("out: ");
        //print_stack(&output);

        if ((formula[i] >= 'a' && formula[i] <= 'z') || formula[i] == '1' || formula[i] == '0')
        {
            push(&output, formula[i]);
        }
        else if (formula[i] == '=')
        {
            if (stack.size > 0)
            {
                while(stack.elements[stack.size - 1] == '~' || stack.elements[stack.size - 1] == '&' || stack.elements[stack.size - 1] == '|' || stack.elements[stack.size - 1] == '>')
                {
                    push(&output, pop(&stack));
                    if (stack.size == 0)
                        break;
                }
            }
            push(&stack, '=');

        }
        else if (formula[i] == '>')
        {
            if (stack.size > 0)
            {
                while(stack.elements[stack.size - 1] == '~' || stack.elements[stack.size - 1] == '&' || stack.elements[stack.size - 1] == '|')
                {
                    push(&output, pop(&stack));
                    if (stack.size == 0)
                        break;
                }
            }

            push(&stack, '>');
        }
        else if (formula[i] == '|')
        {
            //printf("I am here!\n");
            if (stack.size > 0)
            {
                while(stack.elements[stack.size - 1] == '~' || stack.elements[stack.size - 1] == '&' || stack.elements[stack.size - 1] == '|')
                {
                    push(&output, pop(&stack));
                    if (stack.size == 0)
                        break;
                }
            }
            push(&stack, '|');

        }
        else if (formula[i] == '&')
        {
            if (stack.size > 0)
            {
                while(stack.elements[stack.size - 1] == '~' || stack.elements[stack.size - 1] == '&')
                {
                    push(&output, pop(&stack));
                    if (stack.size == 0)
                        break;
                }
            }
            push(&stack, '&');
        }
        else if (formula[i] == '~')
        {
            // jezeli na szczycie nie ma negacji albo stos jest pusty
            if (stack.size > 0)
            {
                if (stack.elements[stack.size - 1] == '~')
                {
                    pop(&stack); // usuwanie parzystej negacji
                }
                else
                {
                    push(&stack, '~');
                }

                /*
                while(stack.elements[stack.size - 1] == '~')
                {
                    push(&output, pop(&stack));
                    if (stack.size == 0)
                        break;
                }
                */
            }
            else
            {
                push(&stack, '~');
            }

        }
        else if (formula[i] == '(')
        {
            push(&stack, '(');
        }
        else if (formula[i] == ')')
        {
            if (stack.size > 0)
            {
                while (stack.elements[stack.size - 1] != '(')
                {
                    push(&output, pop(&stack));
                    if (stack.size == 0)
                        break;
                }

                if (stack.size != 0)
                {
                    pop(&stack);
                }
            }
        }
    }

    while(stack.size > 0)
    {
        push(&output, pop(&stack));
    }

    return output;
}

bool k_bit(int a, int k)
{
    return (a & (1 << k));
}

int max_evaluations(int n)
{
    return (1 << n);
}

unsigned int count_variables(char* formula, int length, char* variables)
{
    bool var[26] = {false};
    //Stack stack = init_stack();

    for (int i = 0; i < length; i++)
    {
        if (formula[i] <= 'z' && formula[i] >= 'a')
        {
            var[formula[i] - 'a'] = true;

            //printf("%c\n", formula[i]);
        }
    }

    unsigned int counter = 0;

    int e = 0;

    for (int i = 0; i < 26; i++)
    {
        if (var[i])
        {
            counter++;
            variables[e] = i + 'a';
            e++;
        }
    }


    return counter;
}


unsigned int find_variable_index(char* variables, char var)
{

//    printf("DEBUG: %c ", var);

    for (unsigned int i = 0; i < 26; i++)
    {
//        printf("%c\n", variables[i]);
        if (variables[i] == var)
        {
            return i;
        }
    }
    return 0;

}

bool evaluate(Stack rpn, int evaluation, char* variables)
{
    // TODO: zamiana zmiennych na a-b
    // TODO: liczenie wartosci

    int n = rpn.size;

    char* array = malloc(sizeof(char) * n);

    // przepisywanie do nowego stosu
    for (int i = 0; i < n; i++)
    {
        array[i] = rpn.elements[i];
    }

    rpn.elements = array;

    for (int i = 0; i < rpn.size; i++)
    {
        if (rpn.elements[i] <= 'z' && rpn.elements[i] >= 'a')
        {
            //printf("elements[%d] = %c", i, rpn.elements[i]);
            if (k_bit(evaluation, find_variable_index(variables, rpn.elements[i])))
            {
                rpn.elements[i] = '1';
            }
            else
            {
                rpn.elements[i] = '0';
            }
        }
    }


    int i = 0;

    while(rpn.size != 1)
    {

        if (!(rpn.elements[i] == '0' || rpn.elements[i] =='1'))
        {
            if (rpn.elements[i] == '~')
            {
                if (rpn.elements[i-1] == '0')
                {
                    rpn.elements[i-1] = '1';
                }
                else if (rpn.elements[i-1] == '1')
                {
                    rpn.elements[i-1] = '0';
                }

                offset_stack(&rpn, 1, i);
                i = 0;
            }
            else
            {
                bool first;
                bool second;

                if (rpn.elements[i-2] == '0')
                {
                    first = false;
                }
                else
                {
                    first = true;
                }

                if (rpn.elements[i-1] == '0')
                {
                    second = false;
                }
                else
                {
                    second = true;
                }

                if (rpn.elements[i] == '>')
                {
                    if (!first || second)
                    {
                        rpn.elements[i-2] = '1';
                    }
                    else
                    {
                        rpn.elements[i-2] = '0';
                    }
                }
                else if (rpn.elements[i] == '=')
                {
                    if (first == second)
                    {
                        rpn.elements[i-2] = '1';
                    }
                    else
                    {
                        rpn.elements[i-2] = '0';
                    }
                }
                else if (rpn.elements[i] == '&')
                {
                    if (first && second)
                    {
                        rpn.elements[i-2] = '1';
                    }
                    else
                    {
                        rpn.elements[i-2] = '0';
                    }
                }
                else if (rpn.elements[i] == '|')
                {
                    if (first || second)
                    {
                        rpn.elements[i-2] = '1';
                    }
                    else
                    {
                        rpn.elements[i-2] = '0';
                    }
                }
                offset_stack(&rpn, 2, i-1);
                i = 0;
            }

        }
        //printf("stack = ");
        //print_stack(&rpn);
        i++;
    }


    if (rpn.elements[0] == '1')
        return true;
    else
        return false;
}

int main()
{
    int n;
    scanf("%d\n", &n);

    int* r = malloc(sizeof(int) * n);

    for (int e = 0; e < n; e++)
    {
        char formula[1000] = {'\0'};

        char c;
        int i = 0;

        while ((c = getchar()) != ';')
        {
            formula[i] = c;
            i++;
        }

        Stack rpn = make_RPN(formula, i);
        int counter = 0;

        char variables[26] = {'\0'};
        int count = count_variables(formula, i, variables);
        int m = max_evaluations(count);

        //printf("Formula #%d\n", e);
        for (int evaluation = 0; evaluation < m; evaluation++)
        {
        //    printf("Evaluation #%d\n", evaluation);
            if(evaluate(rpn, evaluation, variables))
            {
                counter++;
            }

        }

        //printf("i=%d %d\n", e, counter);
        r[e] = counter;
    }

    for (int i = 0; i < n; i++)
    {
        printf("%d\n", r[i]);
    }
}

