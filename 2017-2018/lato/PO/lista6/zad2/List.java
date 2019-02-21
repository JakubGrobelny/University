// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L6, z2, Kolekcja implementuj¹ca interfejs „Collection”.
// Implementacja kolekcji (listy dwukierunkowej) implementuj¹cej
// interfejs „Collection”.
// List.java
// 2018-04-05

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.lang.UnsupportedOperationException;
import java.util.Arrays;

// Lista dwukierunkowa implementuj¹ca interfejs „Collection”.
public class List<E> implements Collection<E>
{
    // Klasa reprezentuj¹ca pojedynczy element listy.
    private class Element<T>
    {
        // Wartoœæ elementu oraz referencje do 
        // nastêpnego i poprzedniego elementu.
        public T value;
        public Element<T> previous;
        public Element<T> next;

        // Konstruktor.
        public Element(T value)
        {
            this.value = value;
            this.previous = null;
            this.next = null;
        }
        
    }

    // Klasa reprezentuj¹ca iterator kolekcji.
    private class ListIterator implements Iterator<E>
    {
        // Referencja do listy, któr¹ przebiega iterator.
        Element<E> current;

        // Metoda sprawdzaj¹ca, czy istnieje element nastêpny.
        public boolean hasNext()
        {
            return (current != null);
        }

        // Metoda zwracaj¹ca kolejny element kolekcji.
        public E next() throws NoSuchElementException
        {
            if (current == null)
                throw new NoSuchElementException();

            E value = current.value;
            current = current.next;
            return value;
        }

        // Metoda (nie jest wymagana przez interfejs)
        // usuwaj¹ca aktualnie wskazywany element.
        public void remove() throws UnsupportedOperationException
        {
            throw new UnsupportedOperationException();
        }

        // Konstruktor
        ListIterator(List<E> list)
        {
            current = list.beginning;
        }
    }

    // Referencja do poczatku i koñca listy.
    private Element<E> beginning;
    private Element<E> end;
    // Iloœæ elementów listy.
    private int size;

    // Metoda zwracaj¹ca iterator listy.
    public Iterator<E> iterator()
    {
        return new ListIterator(this);
    }

    // Metoda sprawdzaj¹ca, czy lista jest pusta.
    public boolean isEmpty()
    {
        return size == 0;
    }

    // Metoda dodaj¹ca nowy element do listy. Zwraca prawdê, je¿eli
    // lista zosta³a zmodyfikowana (w tym przyk³adzie zawsze dodawany
    // jest element).
    public boolean add(E element)
    {
        pushBack(element);
        return true;
    }

    // Metoda usuwaj¹ca wszystkie elementy o wartoœci „element”.
    // Zwraca prawdê, je¿eli lista zosta³a zmodyfikowana.
    public boolean remove(Object element)
    {
        boolean wasRemoved = false;

        List<E> newList = new List<E>();
        
        while(!isEmpty())
        {
            E value = popFront();
            
            if (value.equals(element))
                wasRemoved = true;
            else
                newList.pushBack(value);
        }

        this.beginning = newList.beginning;
        this.end = newList.end;
        this.size = newList.size;

        return wasRemoved;
    }

    // Metoda usuwaj¹ca wszystkie elementy z listy.
    public void clear()
    {
        size = 0;
        beginning = null;
        end = null;
    }

    // Metoda zwracajaca rozmiar listy.
    public int size()
    {
        return size;
    }

    // Metoda sprawdzaj¹ca, czy wszystkie elementy z listy „list”
    // znajduj¹ siê równie¿ w liœcie, dla której wywo³ywana jest metoda.
    @SuppressWarnings("unchecked")
    public boolean containsAll(Collection<?> list)
    {        
        for (E element : (Collection<E>)list)
        {
            Element<E> ptrThis = beginning;

            while (ptrThis != null)
            {
                if (ptrThis.value.equals(element))
                    break;

                ptrThis = ptrThis.next;
            }
            
            if (ptrThis == null)
                return false;
        }

        return true;
    }

    // Metoda dodaj¹ca wszystkie elementy z kolekcji „collection”, które
    // nie znajdowa³y siê na dotychczasowej liscie.
    // Zwraca prawdê, je¿eli lista zosta³a zmodyfikowana.
    public boolean addAll(Collection<? extends E> collection)
    {
        boolean changed = false;

        for (E element : collection)
        {
            boolean exists = false;

            Element<E> ptrThis = beginning;

            while (ptrThis != null)
                if (ptrThis.value.equals(element))
                    exists = true;

            if (!exists)
            {
                pushBack(element);
                changed = true;
            }
        }

        return changed;
    }

    // Metoda usuwaj¹ca wszystkie elementy z kolekcji „list”, które znajduj¹
    // siê w kolekcji, dla której wywo³ana zosta³a metoda.
    // Zwraca prawdê, je¿eli lista zosta³a zmodyfikowana.
    @SuppressWarnings("unchecked")
    public boolean removeAll(Collection<?> list)
    {
        boolean changed = false;
        
        for (E element : (Collection<E>)list)
        {
            int prevSize = size;

            remove(element);

            if (size < prevSize)
                changed = true;
        }

        return changed;
    }

    // Metoda usuwaj¹ca z listy takie elementy, które nie znajduj¹
    // siê w liœcie „list”.
    // Zwraca prawdê, je¿eli lista zosta³a zmodyfikowana.
    public boolean retainAll(Collection<?> list)
    {
        boolean changed = false;

        for (E element : this)
        {
            if (!list.contains(element))
            {
                remove(element);
                changed = true;
            }
        }

        return changed;
    }

    // Metoda zmieniaj¹ca listê na tablicê.
    public Object[] toArray()
    {
        Object[] array = new Object[size];

        Element<E> ptr = beginning;
        int i = 0;

        while (ptr != null)
        {
            array[i++] = ptr.value;
            ptr = ptr.next;
        }

        return array;
    }

    // Metoda zmieniaj¹ca listê na tablicê zadanego typu.
    @SuppressWarnings("unchecked")
    public <T> T[] toArray(T[] array)
    {
        Object[] original = toArray();
        T[] result = Arrays.copyOf(array, original.length);

        for (int i = 0; i < original.length; i++)
            result[i] = (T)original[i];

        return result;
    }

    // Metoda sprawdzaj¹ca, czy zadany element „obj” znajduje siê w liœcie.
    public boolean contains(Object obj)
    {
        if (obj == null)
            return false;

        Element<E> ptr = beginning;
        
        while (ptr != null)
        {
            if (ptr.value.equals(obj))
                return true;

            ptr = ptr.next;
        }
    
        return false;
    }

    // Metoda dodaj¹ca nowy element na pocz¹tek listy.
    public void pushFront(E value)
    {
        size++;

        Element<E> newFront = new Element<E>(value);
        newFront.next = beginning;

        if (newFront.next == null)
            end = newFront;
        else
            beginning.previous = newFront;

        beginning = newFront;
    }

    // Metoda dodaj¹ca nowy element na koniec listy.
    public void pushBack(E value)
    {
        size++;

        Element<E> newEnd = new Element<E>(value);
        newEnd.previous = end;

        if (newEnd.previous == null)
            beginning = newEnd;
        else
            end.next = newEnd;
            
        end = newEnd;
    }

    // Metoda usuwaj¹ca i zwracaj¹ca element z pocz¹tku listy.
    public E popFront() throws IllegalStateException
    {
        size--;

        if (beginning != null)
        {
            Element<E> newFront = beginning.next;

            if (newFront != null)
                newFront.previous = null;
            else
                end = null;
        
            E result = beginning.value;
            
            beginning = newFront;

            return result;
        }
        else
            throw new IllegalStateException("popFront(): list is empty!");
    }

    // Metoda usuwaj¹ca i zwracaj¹ca ostatni element listy.
    public E popBack() throws IllegalStateException
    {
        size--;

        if (end != null)
        {
            Element<E> newEnd = end.previous;

            if (newEnd != null)
                newEnd.next = null;
            else
                beginning = null;

            E result = end.value;

            end = newEnd;

            return result;
        }
        else
            throw new IllegalStateException("popBack(): list is empty!");

    }

    // Metoda zwracaj¹ca element o zadanym indeksie.
    public E get(int index) throws IndexOutOfBoundsException
    {
        if (index >= size)
            throw new IndexOutOfBoundsException("get(): index out of range!");
        else
        {
            int i = 0;

            Element<E> ptr = beginning;

            while (i != index)
            {
                i++;
                ptr = ptr.next;
            }

            return ptr.value;
        }
    }

    // Metoda ustawiaj¹cy element o podanym indeksie na podan¹ wartoœæ.
    public void set(int index, E value) throws IndexOutOfBoundsException
    {
        if (index >= size)
            throw new IndexOutOfBoundsException("set(): index out of range!");
        else
        {
            Element<E> ptr = beginning;
            int i = 0;

            while (i != index)
            {
                ptr = ptr.next;
                i++;
            }

            ptr.value = value;
        }
    }

    // Konstruktor.
    public List()
    {
        this.beginning = null;
        this.end = null;
        this.size = 0;
    }
}