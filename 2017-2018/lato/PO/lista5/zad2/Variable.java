// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z2, Wyra¿enia arytmetyczne.
// Implementacja reprezentacji wyra¿eñ
// arytmetycznych w postaci drzew.
// Variable.java
// 2018-03-29

import java.util.HashMap;

// Klasa reprezentuj¹ca lisæ drzewa zawieraj¹cy zmienn¹.
public class Variable extends Expression
{
    // Pole zawierajace symbol zmiennej.
    private String symbol;

    // Publiczna, statyczna mapa haszowania zawieraj¹ca wartoœci
    // przypisane danym zmiennym.
    public static HashMap<String, Float> variables = new HashMap<String, Float>();
    
    // Publiczna, statyczna metoda s³u¿¹ca do dodawania nowych
    // zmiennych do mapy.
    public static void addVariable(String symbol, float val)
    {
        variables.put(symbol, val);
    }

    // Metoda wyliczaj¹ca wartoœæ zmiennej. Je¿eli zmienna nie istnieje,
    // to wyrzucony zostaje wyj¹tek IllegalArgumentException.
    public float evaluate() throws IllegalArgumentException
    {
        if (variables.get(symbol) != null)
            return variables.get(symbol);
        else
            throw new IllegalArgumentException();
    }

    // Metoda zwracaj¹ca symbol jako ³añcuch znaków.
    public String toString()
    {
        return symbol;
    }

    // Konstruktor.
    public Variable(String symbol)
    {
        this.symbol = symbol;
    }
}