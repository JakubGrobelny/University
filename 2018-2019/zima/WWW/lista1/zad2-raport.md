Legenda:
- X – nie można zagnieżdżać
- puste miejsce - można zagnieżdżać

Kolejne znaczniki z pierwszego wiersza oznaczają zagnieżdżane znaczniki w znacznikach z pierwszej kolumny. 

|          |article |aside|header|footer|address|section|nav|h1|h2|a|
|-|-       |-       |-    |-     |-     |-      |-      |-  |- |- |
|<b>article|        |     |      |      |       |       |   |  |  |
|<b>aside  |        |     |      |      |       |       |   |  |  |
|<b>header |        |     |  X   |  X   |       |       |   |  |  |
|<b>footer |        |     |  X   |  X   |       |       |   |  |  |
|<b>address|   X    |     |  X   |  X   |  X    |   X   | X |X |X |
|<b>section|        |     |      |      |       |       |   |  |  |
|<b>nav    |        |     |      |      |       |       |   |  |  |
|<b>h1     |   X    |  X  |  X   |  X   |  X    | X     | X |X |X |
|<b>h2     |   X    |  X  |  X   |  X   |  X    | X     | X |X |X |
|<b>a      |        |     |      |      |       |       |   |  |  |X

Podsumowanie:
    - Nie można zagnieżdżać innych znaczników wewnątrz znaczników **h1** i **h2**
    - **footer** oraz **header** nie mogą być nawzajem zagnieżdżone
    - w znaczniku **address** nie można zagnieżdżać niczego oprócz **a** i **aside**
    - wszystko może być zagnieżdżone w **a** oraz **a** może być zagnieżdżone we wszystkim z wyjątkiem innego **a**
    - jedynie **article**, **aside**, **section** oraz **nav** mogą być zagnieżdżane w tych samych znacznikach
    - w **nav**, **section**, **aside** oraz **aside** można zagnieżdżać wszystko
