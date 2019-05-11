#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <math.h>


struct point
{
    int x;
    int y;
};


struct pair
{
    int first;
    int second;
    double dist;
};


double min_double(double a, double b)
{
    return a > b ? a : b;
}


int comparator(const void* aptr, const void* bptr)
{
    const struct point a = *((struct point*)aptr);
    const struct point b = *((struct point*)bptr);

    return a.x == b.x
         ? a.y - b.y
         : a.x - b.x;
}


void read_points(int n, struct point* dest)
{
    for (int i = 0; i < n; i++)
        scanf("%d %d", &dest[i].x, &dest[i].y);
}


double points_distance(struct point a, struct point b)
{
    double ax = (double)a.x;
    double ay = (double)a.y;

    double bx = (double)b.x;
    double by = (double)b.y;

    double xdiff = ax - bx;
    double ydiff = ay - by;

    return sqrt(xdiff*xdiff + ydiff*ydiff);
}


struct pair check_all(int b, int e, struct point* points)
{
    struct pair closest = {
        .first = -1,
        .second = -1,
        .dist = __DBL_MAX__
    };

    for (int i = b; i < e; i++)
    {
        for (int j = b; j < e; j++)
        {
            if (i != j)
            {
                double dist = points_distance(points[i], points[j]);

                if (dist < closest.dist)
                {
                    closest.dist = dist;
                    closest.first = i;
                    closest.second = j;
                }
            }
        }
    }

    return closest;
}


struct pair find_best_between(int b, int e, struct point* points)
{
    struct pair closest = {
        .first = -1,
        .second = -1,
        .dist = __DBL_MAX__
    };

    for (int i = b; i < e; i++)
    {
        for (int j = i + 1; j < e && j < i + 7; j++)
        {
            double dist = points_distance(points[i], points[j]);

            if (dist < closest.dist)
            {
                closest.dist = dist;
                closest.first = i;
                closest.second = j;
            }
        }
    }

    return closest;
}



struct pair find_closest(int b, int e, struct point* points)
{
    if (e - b <= 3)
        return check_all(b, e, points);

    int mid = (b + e) / 2;

    struct pair closest_left = find_closest(b, mid, points);
    struct pair closest_right = find_closest(mid, e, points);

    struct pair best_candidate = closest_left.dist < closest_right.dist
                               ? closest_left
                               : closest_right;

    int mid_left;
    int mid_right;

    for (mid_left = mid; mid_left >= b; mid_left--)
    {
        if ((double)(mid - points[mid_left].x) >= best_candidate.dist)
        {
            mid_left++;
            break;
        }
    }    

    for (mid_right = mid; mid_right < e; mid_right++)
    {
        if ((double)(points[mid_right].x - mid) >= best_candidate.dist)
        {
            mid_right--;
            break;
        }
    }

    if (mid_right == mid_left)
        return best_candidate;

    struct pair best_mid = find_best_between(mid_left, mid_right, points);

    return best_mid.dist < best_candidate.dist
         ? best_mid
         : best_candidate;
}


void solve(int n, struct point* points)
{
    qsort(points, n, sizeof(struct point), comparator);

    struct pair closest = find_closest(0, n, points);

    printf("%d %d\n", points[closest.first].x, points[closest.first].y);
    printf("%d %d\n", points[closest.second].x, points[closest.second].y);
}


int main() 
{
    int n;
    scanf("%d", &n);

    struct point* points = malloc(sizeof(struct point) * n);
    read_points(n, points);

    solve(n, points);

    free(points);
    return 0;
}
