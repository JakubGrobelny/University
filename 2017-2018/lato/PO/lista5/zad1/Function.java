// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z1, Kolekcje porównywalnych elementów.
// Hierarchia klas implementuj¹cych interfejs „Comparable<T>”.
// Function.java
// 2018-03-30

// Abstracyjna klasa, z której dziedzicz¹ inne klasy
// reprezentuj¹ce ró¿ne typy funkcji.
public abstract class Function implements Comparable<Function>
{
    // Porównywanie funkcji w tym przyk³adzie polega
    // na sprawdzaniu, która funkcja ma wiêksz¹
    // iloœæ wiêkszych wartoœci na przedziale
    // od zera do tysi¹ca.    
    public int compareTo(Function f)
    {
        int accumulator = 0;
        
        for (float x = 0.0f; x <= 1000.0f; x += 10.0f)
        {
            float y1 = this.valueAt(x);
            float y2 = f.valueAt(x);
            
            if (y1 > y2)
                accumulator++;
            else if (y1 < y2)
                accumulator--;
        }
        
        return accumulator;
    }

    // 
    public abstract String toString();
    
    // Metoda zwracaj¹ca wartoœæ funkcji w punkcie x.
    public abstract float valueAt(float x);
}