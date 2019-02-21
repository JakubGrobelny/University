(** Typ danych reprezentujący formuły zdaniowe *)
type 'a prop = Var of 'a | Top | Bot
             | Conj of 'a prop * 'a prop
             | Disj of 'a prop * 'a prop
             | Impl of 'a prop * 'a prop

(** Typ danych reprezentujący drzewa dowodu w systemie dedukcji
   naturalnej, wraz z typem ramek *)
type 'a pt = Ax of 'a prop
           | TopI
           | ConjI  of 'a pt * 'a pt
           | DisjIL of 'a pt * 'a prop
           | DisjIR of 'a prop * 'a pt
           | ImplI  of 'a hypt
           | BotE   of 'a prop
           | ConjEL of 'a pt
           | ConjER of 'a pt
           | DisjE  of 'a pt * 'a hypt * 'a hypt
           | ImplE  of 'a pt * 'a pt
and 'a hypt = 'a prop * 'a pt

(** Typ danych reprezentujący skryptowy zapis dowodu (patrz lista 5) w
   systemie dedukcji naturalnej, wraz z typem ramek *)
type 'a ps = PDone of 'a prop
           | PConc of 'a prop * 'a ps
           | PHyp  of 'a shyp * 'a ps
and 'a shyp = 'a prop * 'a ps


type 'a thing = TGoal of string * ('a prop) * ('a pt)
              | SGoal of string * ('a prop) * ('a ps)

(** Plik zawiera listę dowodów (jednego lub drugiego rodzaju) ze
   zmiennymi typu string *)
type file = string thing list

