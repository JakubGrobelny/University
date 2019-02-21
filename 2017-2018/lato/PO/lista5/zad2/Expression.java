// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z2, Wyra¿enia arytmetyczne.
// Implementacja reprezentacji wyra¿eñ
// arytmetycznych w postaci drzew.
// Expression.java
// 2018-03-29

// Abstrakcyjna klasa, z której dziedzicz¹ klasy 
// reprezentuj¹ce inne elementy drzewa.
public abstract class Expression
{    
    public abstract float evaluate();    
    public abstract String toString();
}
