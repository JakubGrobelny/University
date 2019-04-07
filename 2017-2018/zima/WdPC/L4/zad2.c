#include <stdio.h>
#include <math.h>
#include <stdbool.h>

// STALE KTORYMI MOZNA MODYFIKOWAC WYGLAD ZEGARA
#define RADIUS 10           //promien tarczy
#define BACKGROUND '.'      //znak bedacy w tle tarczy
#define HOUR '#'            //znak z ktorego sklada sie wskazowka godzinna
#define MINUTES '|'         //znak z ktorego sklada sie wskazowka minutowa
#define HOUR_LENGTH 6.0     //dlugosc wskazowki godzinnej
#define MINUTE_LENGTH 9.0   //dlugosc wskazowki minutowej
#define ANGLE_TOLERANCE 0.1 //tolerancja dokladnosci sinusa i cosinusa wplywajaca na grubosc wskazowki [TEGO LEPIEJ NIE RUSZAC BO TAK JEST DOBRZE]

//funkcja sprawdzajaca czy punkt lezy wewnatrz (lub na) okregu
bool insideCircle(double x, double y)
{
    if (x*x + y*y <= RADIUS*RADIUS)
        return true;
    return false;
}

//funkcja przeliczajaca minuty na kat w radianach
double minutesToRadians(int minutes)
{
    if (!minutes)
        minutes = 60;
    double angle = ((double)(minutes)/60.0) * 2.0 * M_PI;
    return angle;
}

//funkcja przeliczajca godziny na kat w radianach
double hoursToRadians(int hour)
{
    if (!hour)
        hour = 24;
    double angle = ((double)(hour)/12.0) * 2.0 * M_PI;
    return angle;
}

// funkcja sprawdzajaca czy punkt lezy na wskazowce
bool isOnIndicator(double x, double y,bool hour, int time)
{
    double distanceFromTheMiddle;
    distanceFromTheMiddle = sqrt(x*x + y*y); // zmienna rowna odleglosci punktu od srodka zegara
    double angle;

    if (hour)
    {
        if (distanceFromTheMiddle > HOUR_LENGTH) // jezeli dlugosc wskazowki godzinnej jest mniejsza niz odleglosc punktu od srodka to nie rysujemy
            return false;
        angle = hoursToRadians(time); // liczenie kata kiedy podano godzine
    }
    else
    {
        if (distanceFromTheMiddle > MINUTE_LENGTH) // analogicznie do godzin
            return false;
        angle = minutesToRadians(-time);

    }

    // wyznaczanie sinusa i cosinusa kata odcinka (0,0) do (x, y) korzystac z faktu, ze cosinus jest to wspolrzedna x punktu na okregu o promieniu 1
    // a sinus to wspolrzedna y punktu na takim okregu. Odleglosc od srodka moze nie byc rowna 1 wiec dzielimy te wspolrzedne aby odpowiadaly takim
    // lezacym na okregu o promieniu 1
    double pointCos = x / distanceFromTheMiddle;
    double pointSin = y / distanceFromTheMiddle;

    // liczenie dokladnego sinusa i cosinusa z funkcji z math.h
    double accurateCos = cos(angle);
    double accurateSin = sin(angle);

    // mnoznik majacy na celu "splaszczyc" wskazowke u podstawy (normalnie wyglada jak wycinek kola :/)
    double multiplier = distanceFromTheMiddle / 5;

    // sprawdzanie, czy kat odcinka jest wystarczajaco bliski dokladnemu katowi dla danej minuty/godziny
    if (pointCos < accurateCos + ANGLE_TOLERANCE / multiplier &&
        pointCos > accurateCos - ANGLE_TOLERANCE / multiplier &&
        pointSin < accurateSin + ANGLE_TOLERANCE / multiplier &&
        pointSin > accurateSin - ANGLE_TOLERANCE / multiplier)
        return true;
    return false;
}

//funkcja rysujaca tarcze ze wskazowkami
void drawClock(int hour, int minutes)
{
    // sprawdzamy punkty lezace w prostokacie o wymiarach srednica tarczy x srednica tarczy
    for (int y = RADIUS; y >= -RADIUS; y--)
    {
        for (int x = -RADIUS; x <= RADIUS; x++)
        {
            // sprawdzanie czy srodki kwadratow 1x1 na jakie dzieli sie powierzchnia leza wewnatrz tarczy
            if (insideCircle(y - 0.5, x + 0.5))
            {
                // sprawdzanie czy punkt lezy na wskazowce w takiej kolejnosci aby dluzsza wskazowka minutowa nie zaslaniala godzinnej
                if (isOnIndicator(y - 0.5, x + 0.5, true, hour))
                    putchar(HOUR);
                else if (isOnIndicator(y - 0.5, x + 0.5, false, -minutes))
                    putchar(MINUTES);
                else
                    putchar(BACKGROUND);
            }
            else
                putchar(' ');
        }
        putchar('\n');
    }
}

int main()
{
    int hour;
    int minutes;

    printf("Podaj godzine i minuty oddzielone spacja: ");
    scanf("%d %d", &hour, &minutes);

    drawClock(hour, minutes);

    return 0;
}
