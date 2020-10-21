# Zadanie 3


1. $S \rightarrow (S) \,|\, SS \,|\, \varepsilon$
2. $S \rightarrow (S)S \,|\, \varepsilon$

Pokażemy, że każde słowo $w \in L_1$ należy do $L_2$.
Dowód poprzez indukcję względem słowa $w$.
1. $w = \varepsilon$ – trywialne.
2. Weźmy dowolne $w > \varepsilon$ i rozważmy jego możliwe postacie. Załóżmy, że każde słowo mniejsze niż $w$ należy do $L_2$.
    1) $w = (a)$ – Przy użyciu drugiej gramatyki można wyprowadzić takie słowo $S \rightarrow (S)S \rightarrow (S)\varepsilon \rightarrow (S)$.
    2) $w = ab$. Rozważmy przypadki:
        - $a = \varepsilon$ – wówczas $w = b$ więc $w \in L_2$.
        - $a = (a')a''$, więc $w = (a')a''b$. Niech $b' = a''b$. Mamy $w = (a')b'$, co jest wprost wyprowadzalne z produkcji $S \rightarrow (S)S$.

W drugą stronę jest oczywiste: w pierwszej gramatyce możemy uzyskać produkcję z drugiej w następujący sposób:
$$S \rightarrow SS \rightarrow (S)S$$

Dowód jednoznaczności drugiej gramatyki:
1. $\varepsilon$ ma jednoznaczne wyprowadzenie – oczywiste.
2. Weźmy dowolne $w$ i załóżmy, że każde słowo mniejsze niż $w$ ma jednoznaczne wyprowadzenie. Skoro $w$ jest niepuste, to musi być postaci $(w'$. Jest tylko jedna pasująca produkcja, oznacza to, że $w = (a)b$. Skoro $a$ i $b$ mają jednoznaczne wyprowadzenia, to $w$ musi mieć jednoznaczne wyprowadzenie, bo słowo $(a)b$ można uzyskać w tylko jeden sposób.