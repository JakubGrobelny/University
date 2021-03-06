// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z2, Wyrażenia arytmetyczne.
// Implementacja reprezentacji wyrażeń
// arytmetycznych w postaci drzew.
// Expression.java
// 2018-03-29

// Abstrakcyjna klasa, z której dziedziczą klasy 
// reprezentujące inne elementy drzewa.
public abstract class Expression
{    
    public abstract float evaluate();    
    public abstract String toString();
}
