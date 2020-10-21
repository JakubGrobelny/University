# Zadanie 5

`E → id | (E) | E + E | E − E | E ∗ E | E ^ E`



```
Expr = Expr + MultExpr | Expr - MultExpr | MultExpr

MultExpr = MultExpr * PowExpr | PowExpr

PowExpr = AtomicExpr ^ PowExpr | AtomicExpr

AtomicExpr = id | (Expr)
```
