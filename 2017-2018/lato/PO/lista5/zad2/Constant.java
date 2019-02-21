// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z2, Wyra¿enia arytmetyczne.
// Implementacja reprezentacji wyra¿eñ
// arytmetycznych w postaci drzew.
// Constant.java
// 2018-03-29

// Klasa reprezentuj¹ca liœæ drzewa zawieraj¹cy sta³¹.
public class Constant extends Expression
{
    // Pole zawieraj¹ce wartoœæ sta³ej.
    private float value;

    // Metoda zwracaj¹ca wartoœæ sta³ej.
    public float evaluate()
    {
        return value;
    }

    // Metoda zamieniaj¹æa sta³¹ na ³añcuch znaków.
    public String toString()
    {
        return Float.toString(value);
    }

    // Konstruktor
    public Constant(float value)
    {
        this.value = value;
    }
}