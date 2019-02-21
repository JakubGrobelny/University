// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z1, Kolekcje porównywalnych elementów.
// Implementacja list uporz¹dkowanych rosn¹co, które zawieraj¹ elementy
// implementuj¹ce interfejst „Comparable<T>”.
// Element.java
// 2018-03-30

// Klasa reprezentuj¹ca pojedynczy element listy.
public class Element<T extends Comparable<T>>
{
    // Pola zawieraj¹ce wartoœæ oraz referencjê do kolejnego elementu.
    private T value;
    private Element<T> next;

    // Metoda zwracaj¹ca wartoœæ prywatnego pola „value”.
    public T getValue()
    {
        return this.value;
    }

    // Metoda zmieniaj¹ca wartoœæ prywatnego pola „value”.
    public void setValue(T value)
    {
        this.value = value;
    }

    // Metoda zwracaj¹ca wartoœæ prywatnego pola „next”.
    public Element<T> getNext()
    {
        return this.next;
    }

    // Metoda zmieniaj¹ca wartoœæ prywatnego pola „next”.
    public void setNext(Element<T> next)
    {
        this.next = next;
    }

    // Konstruktor
    public Element(T value, Element<T> next)
    {
        this.value = value;
        this.next = next;
    }
}
