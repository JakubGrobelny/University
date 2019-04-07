#include <stdio.h>
#include <stdbool.h>
#include <gtk/gtk.h>
#include <cairo.h>

// switch case wymagal zeby obiekty nie byly zmiennymi wiec zmienilem na #define
#define DROGA '#'
#define OSIEDLE 'O'
#define KOPALNIA 'K'
#define ELEKTROWNIA 'E'
#define LINIA 'T'
#define UNIWERSYTET 'U'

int width, height;

char map[100][100];
bool visited[100][100];

int workers[100][100]; // tablica obiektow ktore maja pracownikow lub ktore ich zapewniaja
int electricity[100][100]; // tablica obiektow ktore maja prad lub go rozprowadzaja
int active[100][100]; // tablica obiektow ktore maja wegiel lub go dostarczaja

bool pathExistsRec(int startX, int startY, char target, char roads, char start)
{
    if (map[startX][startY] == target)
    {
        if (target == OSIEDLE) // szukanie pracownikow
        {
            if (!workers[startX][startY])
            {
                workers[startX][startY] = 1;
                return true;
            }
        }
        if (target == KOPALNIA) // szukanie wegla
        {
            if (!active[startX][startY]) // jezeli kopalnia nie dostarcza wegla
            {
                if (workers[startX][startY]) // jezeli kopalnia ma pracownikow
                {
                    active[startX][startY] = 1;
                    return true;
                }
            }
        }
        if (target == ELEKTROWNIA) // szukanie elektrowni
        {
            if (electricity[startX][startY] < 5) // jezeli podlaczylo sie mniej niz 5 obiektow
            {
                if (workers[startX][startY]) // jezeli elektrownia ma pracownikow
                {
                    if (active[startX][startY]) // jezeli elektrownia ma wegiel
                    {
                        electricity[startX][startY]++;
                        return true;
                    }
                }
            }
        }
    }

    if (visited[startX][startY])
    {
        return false;
    }

    if (map[startX][startY] != roads && map[startX][startY] != start)
    {
        return false;
    }

    visited[startX][startY] = true;


    if (startX > 0) // lewo
    {
        if (pathExistsRec(startX - 1, startY, target, roads, start))
            return true;
    }
    if (startX < width - 1) // prawo
    {
        if (pathExistsRec(startX + 1, startY, target, roads, start))
            return true;
    }
    if (startY > 0) // gora
    {
        if (pathExistsRec(startX, startY - 1, target, roads, start))
            return true;
    }
    if (startY < height - 1) // dol
    {
        if (pathExistsRec(startX, startY + 1, target, roads, start))
            return true;
    }

    //visited[startX][startY] = false;

    return false;
}


bool pathExists(int startX, int startY, char target, char roads, char start)
{
    for (int i = 0; i < 100; i++)
    {
        for (int e = 0; e < 100; e++)
        {
            visited[i][e] = false;
        }
    }

    return pathExistsRec(startX, startY, target, roads, start);
}

void findWorkers(int x, int y, char start)
{
    if (pathExists(x, y, OSIEDLE, DROGA, start))
    {
        workers[x][y] = 1;
    }
}

void findMine(int x, int y)
{
    if (pathExists(x, y, KOPALNIA, DROGA, ELEKTROWNIA))
    {
        active[x][y] = 1;
    }
}

void findElectricity(int x, int y, char start)
{
    if (pathExists(x, y, ELEKTROWNIA, LINIA, start))
    {
        electricity[x][y] = 1;
    }
}

void draw(GtkWidget* window, GdkPixbuf* pix, int x, int y)
{
    cairo_t *cr;
    cr = gdk_cairo_create (gtk_widget_get_window(window));
    gdk_cairo_set_source_pixbuf(cr, pix, 32 * y, 32 * x);
    cairo_paint(cr);
    cairo_destroy (cr);
}

static gboolean on_window_draw(GtkWidget *window, GdkEvent *event, gpointer data)
{
    (void)event; (void)data;
    GdkPixbuf *pix;
    GError *err = NULL;

    for (int i = 0; i < height; i++)
    {
        for (int e = 0; e < width; e++)
        {
            switch (map[e][i])
            {
                case UNIWERSYTET:
                {
                    if (workers[e][i] && electricity[e][i])
                        pix = gdk_pixbuf_new_from_file("pictures/university.png", &err);
                    else
                        pix = gdk_pixbuf_new_from_file("pictures/university2.png", &err);
                    break;
                }
                case ELEKTROWNIA:
                {
                    if (workers[e][i] && active[e][i])
                        pix = gdk_pixbuf_new_from_file("pictures/pplant.png", &err);
                    else
                        pix = gdk_pixbuf_new_from_file("pictures/pplant2.png", &err);
                    break;

                }
                case KOPALNIA:
                {
                    if (workers[e][i])
                        pix = gdk_pixbuf_new_from_file("pictures/mine.png", &err);
                    else
                        pix = gdk_pixbuf_new_from_file("pictures/mine2.png", &err);
                    break;

                }
                case OSIEDLE:
                {
                    if (electricity[e][i])
                        pix = gdk_pixbuf_new_from_file("pictures/house.png", &err);
                    else
                        pix = gdk_pixbuf_new_from_file("pictures/house2.png", &err);
                    break;
                }
                case LINIA:
                {
                    pix = gdk_pixbuf_new_from_file("pictures/pline.png", &err);
                    break;
                }
                case DROGA:
                {
                    pix = gdk_pixbuf_new_from_file("pictures/road.png", &err);
                    break;
                }
                case '.':
                {
                    pix = gdk_pixbuf_new_from_file("pictures/empty.png", &err);
                    break;
                }
                default:
                {
                    pix = gdk_pixbuf_new_from_file("pictures/tree.png", &err);
                    break;
                }
            }

            draw(window, pix, i, e);
        }
    }
    /*
    if(err)
    {
        printf("Error : %s\n", err->message);
        g_error_free(err);
        return FALSE;
    }
    */

}

int main(int argc, char* argv[])
{

    scanf("%i%i", &width, &height);

    for (int y = 0; y < height; y++)
    {
        for (int x = 0; x < width; x++)
        {
            scanf(" %c", &map[x][y]);
            active[x][y] = 0;
            workers[x][y] = 0;
            electricity[x][y] = 0;
        }
    }

    /*
                 UWAGA UWAGA
                 UWAGA

                 Algorytm nie przydziala pracownikow optymalnie, wiec mozliwe jest ze beda oni we wszystkich
                 budynkach oprocz kopalni, przez co cale miasto i tak nie bedzie mialo pradu
                 (ale przynajmniej bezrobocie bedzie niskie)
    */

    for (int i = 0; i < width; i++) // przyporzadkowywanie pracownikow
    {
        for (int e = 0; e < height; e++)
        {
            if (map[i][e] == ELEKTROWNIA) // wiem ze to mogl byc jeden if ale nie chce mi sie teraz tego zmieniac :(
                findWorkers(i, e, ELEKTROWNIA);
            else if (map[i][e] == KOPALNIA)
                findWorkers(i, e, KOPALNIA);
            else if (map[i][e] == UNIWERSYTET)
                findWorkers(i, e, UNIWERSYTET);
        }
    }

    for (int i = 0; i < width; i++) // przyporzadkowywanie kopalni do elektrowni
    {
        for (int e = 0; e < height; e++)
        {
            if (map[i][e] == ELEKTROWNIA)
                findMine(i, e);
        }
    }

    for (int i = 0; i < width; i++) // rozprowadzanie pradu z elektrowni do innych obiektow
    {
        for (int e = 0; e < height; e++)
        {
            if (map[i][e] == UNIWERSYTET || map[i][e] == OSIEDLE)
                findElectricity(i, e, map[i][e]);
        }
    }

    printf("\n\n================DZIALAJACE====================\n\n");

    for (int i = 0; i < height; i++)
    {
        for (int e = 0; e < width; e++)
        {
            switch (map[e][i])
            {
                case UNIWERSYTET:
                    if (workers[e][i] && electricity[e][i])
                        printf("U");
                    else
                        printf("u");
                    break;
                case ELEKTROWNIA:
                    if (workers[e][i] && active[e][i])
                        printf("E");
                    else
                        printf("e");
                    break;
                case KOPALNIA:
                    if (workers[e][i])
                        printf("K");
                    else
                        printf("k");
                    break;
                case OSIEDLE:
                    if (electricity[e][i])
                        printf("O"); // osiedle jest aktywne kiedy ma prad, niekoniecznie kiedy jest praca
                    else
                        printf("o");
                    break;
                default:
                    printf("%c",map[e][i]);
                    break;
            }
        }
        printf("\n");
    }

    /*
    printf("\n\n================ELEKTRYCZNOSC=================\n\n");

    for (int i = 0; i < height; i++)
    {
        for (int e = 0; e < width; e++)
        {
            printf("%d",electricity[e][i]);
        }
        printf("\n");
    }

    printf("\n\n================PRACOWNICY====================\n\n");

    for (int i = 0; i < height; i++)
    {
        for (int e = 0; e < width; e++)
        {
            printf("%d",workers[e][i]);
        }
        printf("\n");
    }

    printf("\n\n================WEGIEL=======================#\n\n");

    for (int i = 0; i < height; i++)
    {
        for (int e = 0; e < width; e++)
        {
            printf("%d",active[e][i]);
        }
        printf("\n");
    }
    */

    // WYSWIETLANIE W GTK+ 3.0

    GtkWidget *window;
    GtkWidget *canvas;
    gtk_init (&argc , &argv);
    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_default_size(GTK_WINDOW(window), 32 * width, 32 * height);
    gtk_window_set_resizable(GTK_WINDOW(window), FALSE);

    g_signal_connect (window, "destroy", G_CALLBACK (gtk_main_quit) , NULL);
    canvas = gtk_drawing_area_new();
    gtk_container_add(GTK_CONTAINER (window), canvas);
    g_signal_connect (canvas, "draw", (GCallback) on_window_draw, NULL);

    gtk_widget_set_app_paintable(canvas, TRUE);
    gtk_widget_show_all (window);
    gtk_main ();

    return 0;
}
