// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z1, Kolekcje porównywalnych elementów.
// Hierarchia klas implementuj¹cych interfejs „Comparable<T>”.
// Rational.java
// 2018-03-30

// Klasa reprezentuj¹ca funkcjê wymiern¹.
public class Rational extends Function
{
    // Wielomiany bêd¹ce licznikiem i mianownikiem funkcji wymiernej.
    Polynomial numerator;
    Polynomial denominator;

    // Metoda zamieniaj¹ca funkcjê na napis.
    public String toString()
    {
        return "( " + numerator.toString() + " ) / (" + denominator.toString() + " )";
    }

    // Metoda obliczaj¹ca wartoœæ funkcji w punkcie x.
    public float valueAt(float x)
    {
        // Je¿eli mianownik jest równy zero to zwrócona zostanie
        // wartoœæ Float.POSITIVE_INFINITY.
        return this.numerator.valueAt(x) / this.denominator.valueAt(x);
    }

    // Konstruktory.
    public Rational(float[] coeffsNum, float[] coeffsDen)
    {
        this.numerator = new Polynomial(coeffsNum);
        this.denominator = new Polynomial(coeffsDen);
    }

    public Rational(Polynomial numerator, Polynomial denominator)
    {
        this.numerator = numerator;
        this.denominator = denominator;
    }
}