// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L6, z5, Wielow¹tkowe sortowanie przez scalanie.
// Implementacja sortowania przez scalanie wykorzystuj¹cego wielow¹tkowoœæ.
// MergeSort.java
// 2018-04-04

// Klasa, w której znajduje siê metoda sort().
public final class MergeSort
{
    // Podklasa rozszerzaj¹ca klasê „Thread”, umo¿liwiaj¹ca jednoczesne
    // sortowanie dwóch czêœci tablicy.
    static class SortingThread extends Thread
    {
        // Referencja do sortowanej tablicy:
        private int[] array;

        // Pola, w których przechowywane s¹ informacje o indeksie
        // od którego zaczyna i na którym koñczy siê dana czêœæ ca³ej tablicy.
        private int start;
        private int end;

        // Przeci¹¿ona, odziedziczona metoda, która 
        // wykonywana jest po uruchomieniu w¹tku.
        // Metoda ta wykonuje sortowanie przez scalanie.
        public void run()
        {
            if (start == end)
                return;
            else
            {
                int middle = (start + end) / 2;
                Thread[] threads = new Thread[2];

                // Tworzenie oddzielnych w¹tków, które bêd¹ oddzielnie
                // sortowaæ obie czêœci tablicy.
                threads[0] = new SortingThread(array, start, middle);
                threads[1] = new SortingThread(array, middle + 1, end);
                
                for (Thread thread : threads)
                    thread.start();
                
                try
                {
                    // Wymuszanie oczekiwania na zakoñczenie pracy obu
                    // w¹tków przed scalaniem.
                    for (Thread thread : threads)
                        thread.join();
                }
                catch (Exception exc)
                {
                    System.out.println(exc);
                    System.exit(1);
                }

                merge(array, start, middle + 1, middle, end);
            }
        }

        // Konstruktor
        public SortingThread(int[] array, int start, int end)
        {
            this.array = array;
            this.start = start;
            this.end = end;
        }
    }

    // Publiczna statyczna metoda, która inicjuje sortowanie.
    // Metoda ta zwraca now¹ tablicê zamiast modyfikowaæ tê, która
    // zosta³a podana jako parametr.
    public static int[] sort(int[] array)
    {
        // Tworzenie kopii tablicy.
        int[] copy = new int[array.length];
        System.arraycopy(array, 0, copy, 0, array.length);

        // Rozpoczecie sortowania.
        Thread sorting = new SortingThread(copy, 0, copy.length - 1);
        sorting.start();

        try
        {
            // Wymuszenie oczekiwania na zakoñczenie pracy w¹tku.
            sorting.join();
        }
        catch (Exception exc)
        {
            System.out.println(exc);
            System.exit(1);
        }

        return copy;
    }
 
    // Statyczna metoda scalaj¹ca dwie czêœci tablicy.
    private static void merge(int[] array, int s1, int s2, int e1, int e2)
    {
        // Tymczasowa tablica, w której bêdzie przechowywany
        // scalony fragment tablicy.
        int[] temp = new int[e2 - s1 + 1];
        
        int ptr1 = s1;
        int ptr2 = s2;
        int i = 0;
                
        // Przebieganie dwóch czêœci tablicy przy u¿yciu dwóch
        // indeksów i dodawanie elemeentów do tablicy „temp”
        // w kolejnoœci rosn¹cej.
        while (ptr1 <= e1 && ptr2 <= e2)
        {
            if (array[ptr1] >= array[ptr2])
            {
                temp[i] = array[ptr2];
                ptr2++;
            }
            else
            {
                temp[i] = array[ptr1];
                ptr1++;
            }
            
            i++;
        }
        
        int ptr = (ptr1 <= e1) ? ptr1 : ptr2;
        int end = (ptr1 <= e1) ? e1 : e2;

        // Przepisywanie reszty elementów, je¿eli tablice
        // nie by³y tego samego rozmiaru.
        while (ptr <= end)
        {
            temp[i] = array[ptr];
            ptr++;
            i++;
        }

        // Przepisywanie wyniku z „temp” do oryginalnej tablicy.
        ptr = s1;
        for (int element : temp)
        {
            array[ptr] = element;
            ptr++;
        }
    }
}