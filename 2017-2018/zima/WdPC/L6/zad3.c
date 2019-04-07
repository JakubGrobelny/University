#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int cut_value(int value, int min, int max)
{
    if (value < min)
        return min;
    if (value > max)
        return max;
    return value;
}

void remove_neighbours(bool** picture, int width, int height, int x, int y)
{
    picture[x][y] = false;

    for (int x1 = cut_value(x-1, 0, width-1); x1 <= cut_value(x+1, 0, width-1); x1++)
        for (int y1 = cut_value(y-1, 0, height-1); y1 <= cut_value(y+1, 0, height-1); y1++)
            if (picture[x1][y1])
                remove_neighbours(picture, width, height, x1, y1);
}

int main()
{
    char name[2];
    scanf("%s", name);

    int width;
    int height;

    scanf("%d %d", &width, &height);

    bool** picture;
    picture = (bool**)malloc(sizeof(bool*) * width);

    for (int x = 0; x < width; x++)
         picture[x] = (bool*)malloc(sizeof(bool) * height);

    for (int y = 0; y < height; y++)
    {
        for (int x = 0; x < width; x++)
        {
            int temp;
            scanf("%d", &temp);

            picture[x][y] = temp;
        }
    }

    int counter = 0;

    for (int x = 0; x < width; x++)
        for (int y = 0; y < height; y++)
            if (picture[x][y])
            {
                counter++;
                remove_neighbours(picture, width, height, x, y);
            }

    for (int i = 0; i < width; i++)
        free(picture[i]);
    free(picture);

    printf("%d", counter);

    return 0;

}

