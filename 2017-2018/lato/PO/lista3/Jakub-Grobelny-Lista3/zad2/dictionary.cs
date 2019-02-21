// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L3, z2, S³ownik
// Modu³ z implementacj¹ typu s³ownikowego
// dictionary.cs
// 2018-03-14

namespace Dictionaries
{
    // Klasa Dictionary<K, V> reprezentuj¹ca s³ownik, czyli
    // tablicê, która mo¿e byæ indeksowana wartoœciami
    // dowolnego typu.
    public class Dictionary<K, V> where K : class
    {
        // Prywatne pola klasy, oznaczajace kolejno
        // pojemnoœæ tablic, iloœæ elementów oraz tablice
        // kluczy i wartoœci.
        private int capacity;
        private int size;
        private K[] keys;
        private V[] values;

        // Operator indeksu
        public V this[K key]
        {
            get
            {
                return GetValue(key);
            }
            set
            {
                SetValue(key, value);
            }
        }   
        
        // Metoda szukaj¹ca wartoœci znajduj¹cej siê
        // pod indeksem 'key'.
        private V GetValue(K key)
        {
            for (int i = 0; i < size; i++)
            {
                if (keys[i].Equals(key))
                    return values[i];
            }

            throw new System.IndexOutOfRangeException();
        }

        // Metoda ustawiaj¹ca wartoœæ pod indeksem 'key' 
        // na 'value'.
        private void SetValue(K key, V value)
        {
            for (int i = 0; i < size; i++)
            {
                if (keys[i].Equals(key))
                {
                    values[i] = value;
                    return;
                }
            }

            AddElement(key, value);
        }

        // Metoda dodaj¹ca nowy element do s³ownika.
        private void AddElement(K key, V value)
        {
            // Je¿eli iloœæ elementów przekroczy³aby pojemnoœæ
            // tablic, to nale¿y je powiêkszyæ przed dodaniem
            // nowego elementu.
            if (size >= capacity)
            {
                capacity *= 2;
                K[] new_keys = new K[capacity];
                V[] new_values = new V[capacity];

                for (int i = 0; i < size; i++)
                {
                    new_keys[i] = keys[i];
                    new_values[i] = values[i];
                }

                values = new_values;
                keys = new_keys;
            }

            // Dodawanie elementu.
            keys[size] = key;
            values[size] = value;

            size++;
        }

        // Metoda zwracaj¹ca i-ty klucz s³ownika
        public K GetKey(int i)
        {
            return keys[i];
        }

        // Metoda usuwaj¹ca element o kluczu 'key'
        public void Remove(K key)
        {
            for (int index = 0; index < size; index++)
            {
                if (keys[index].Equals(key))
                {
                    for (int i = index; i < size - 1; i++)
                    {

                        keys[i] = keys[i + 1];
                        values[i] = values[i + 1];
                    }

                    size--;
                    return;
                }
            }
        }

        // Konstruktor tworzacy s³ownik o zadanej pojemnoœci
        // (domyœlnie 1)
        public Dictionary(int initialCapacity = 1)
        {
            size = 0;
            capacity = initialCapacity;
            keys = new K[capacity];
            values = new V[capacity];
        }

        // Metoda zwracaj¹ca iloœæ elementów w s³owniku
        public int GetSize()
        {
            return size;
        }
    }
}