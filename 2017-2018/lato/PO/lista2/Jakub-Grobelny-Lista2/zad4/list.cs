// Jakub Grobelny 300481
// 7 marca 2018
// Programowanie Obiektowe - Lista 2, zadanie 4


// Implementacja list leniwych.

// Klasa LazyList:
// Wywołanie metody Element(i) zwraca i-ty element listy.
// Jeżeli lista ma mniej niż i elementów, to zostaje powiększona.
// Wywołanie metody Size() zwraca rozmiar listy.

// Klasa Prime (dziedziczy z LazyList):
// Wywołanie metody Element(i) zwraca i-tą liczbę pierwszą.

using System.Collections.Generic;
using System;

namespace LazyLists
{
    class LazyList
    {
        protected List<int> list;
        
        public LazyList()
        {
            list = new List<int>();
        }

        public int Size()
        {
            return list.Count;
        }

        virtual public int Element(int i)
        {
            if (list.Count <= i)
            {
                Random randomizer = new Random();

                for (int e = list.Count; e <= i; e++)
                {
                    list.Add(randomizer.Next());
                }
            }

            return list[i];
        }
    }


    class Prime : LazyList
    {
        private bool IsPrime(int x)
        {
            foreach (var p in list)
                if (x % p == 0)
                    return false;

            return true;
        }

        public Prime() : base() 
        {
            list.Add(2);
        }

        override public int Element(int i)
        {
            if (list.Count <= i)
            {
                var primeCandidate = list[list.Count - 1] + 1;

                while (list.Count <= i)
                {
                    if (IsPrime(primeCandidate))
                        list.Add(primeCandidate);

                    primeCandidate++;
                }
            }

            return list[i];
        }
    }
}