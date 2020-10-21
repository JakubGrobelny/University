# Zadanie 4

Gramatyka z zadania:

$$S \rightarrow (S) \,|\, [S] \,|\, SS \,|\, \varepsilon$$

Jednoznaczna gramatyka:

$$S \rightarrow (S)S \,|\, [S]S \,|\, \varepsilon$$

Dowód przez indukcję względem słów:

1. $\varepsilon$ ma jednoznaczne wyprowadzenie – oczywiste, tylko jedno wyprowadzenie pasuje.

2. Załóżmy, że podsłowa słowa $w > \varepsilon$ należące do języka mają jednoznaczne wyprowadzenie. Skoro słowo $w > \varepsilon$, to musiało powstać w wyniku zaaplikowania jednej z dwóch produkcji: $(S)S$ lub $[S]S$. Jest więc postaci $(a)b$ lub $[a]b$, gdzie $a$ i $b$ są słowami należącymi do języka, które, z założenia, mają jednoznaczne wyprowadzenia. Niepuste słowo $w$ zaczyna się albo $[$ albo $($, i do obu tych przypadków pasuje tylko jedna produkcja, więc dla obu przypadków mamy jednoznaczy wybór produkcji. Skoro podsłowa również są wyprowadzane jednoznacznie, to całe słowo $w$ również ma jednoznaczne wyprowadzenie.
