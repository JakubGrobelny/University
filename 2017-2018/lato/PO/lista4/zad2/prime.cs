// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L4, z2, Kolekcja liczb pierwszych
// Implementacja kolekcji liczb pierwszych, któr¹
// mo¿na przegl¹daæ przy u¿yciu pêtli „foreach”.
// prime.cs
// 2018-03-21

using System.Collections;

// Przestrzeñ nazw Primes
namespace Primes
{

    // Klasa reprezentuj¹ca kolekcjê liczb pierwszych.
    // (Implementuje interfejs „IEnumerable”.)
    public class PrimeCollection : IEnumerable
    {
        // Prywatna podklasa implementuj¹ca iterator kolekcji.
        public class PrimeEnumerator : IEnumerator
        {
            // Ostatnio wyznaczony element kolekcji. Kolejne liczby
            // pierwsze s¹ wyznaczane na bierz¹co i nie s¹ nigdzie
            // przechowywane.
            int current;

            // Predykat sprawdzaj¹cy pierwszoœæ zmiennej „current”.
            private bool IsPrime()
            {            
                if (this.current % 2 == 0 && this.current != 2)
                    return false;

                for (int i = 3; i <= (int)System.Math.Sqrt(this.current); i += 2)
                    if (this.current % i == 0)
                        return false;

                return true;
            }

            // Konstruktor (ustawia pole „current” na 1). 
            // (iterator zaczyna przebiegaæ kolekcjê od
            // 'minus pierwszego' elementu.)
            public PrimeEnumerator()
            {
                this.Reset();
            }

            // Metoda wymagana przez interfejs „IEnumerator”. Znajduje kolejny
            // element w kolekcji. Je¿eli nie ma kolejnego elementu, to
            // zwraca fa³sz. W przeciwnym razie zwraca prawdê.
            public bool MoveNext()
            {
                this.current++;

                while (!IsPrime())
                {
                    if (this.current == System.Int32.MaxValue)
                        return false;
                    this.current++;
                }

                return true;
            }

            // Ustawienie iteratora z powrotem na pocz¹tkowy element.
            public void Reset()
            {
                this.current = 1;
            }

            // W³aœciwoœæ „Current” wymagana przez interfejs.
            object IEnumerator.Current
            {
                get { return Current; }
            }

            // W³aœciwoœæ pozwalaj¹ca na dostêp do aktualnego elementu.
            public int Current
            {
                get { return this.current; }
            }
        }

        // Konstruktor kolekcji.
        public PrimeCollection() {}

        // Metoda wymagana przez interfejs, zwracaj¹ca iterator.
        IEnumerator IEnumerable.GetEnumerator()
        {
            return (IEnumerator)GetEnumerator();
        }

        // Implementacja metody tworz¹cej iterator.
        public PrimeEnumerator GetEnumerator()
        {
            return new PrimeEnumerator();
        }
    }

}