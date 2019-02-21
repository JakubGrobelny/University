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

using System;
using LazyLists;

namespace ListTest
{
    class MainClass
    {
        public static void Main() 
        {
            var primeList = new Prime();
            var lazyList = new LazyList();

            bool exit = false;

            while (!exit)
            {
                Console.WriteLine("Choose an option: ");
                Console.WriteLine("1) Get an element from LazyList.");
                Console.WriteLine("2) Get an element from PrimeList.");
                Console.WriteLine("3) Exit the program.");
                Console.WriteLine();

                var choice = Console.ReadKey(true).KeyChar;

                switch (choice)
                {
                    case '1':
                        printListInformation(lazyList);
                        break;
                    case '2':
                        printListInformation(primeList);
                        break;
                    case '3':
                        exit = true;
                        break;
                    default:
                        Console.WriteLine("There is no such option!");
                        Console.WriteLine("Press any key.");
                        Console.ReadKey();
                        break;
                }

                Console.Clear();
            }
        }

        public static void printListInformation(LazyList list)
        {
            Console.WriteLine("Enter the index of the element: ");

            int index;
            bool invalid = false;

            if (!int.TryParse(Console.ReadLine(), out index))
                invalid = true;
            else if (index < 0)
                invalid = true;

            if (invalid)
            {
                Console.WriteLine("Invalid input!");
                
                Console.WriteLine("Press any key to continue.");
                Console.ReadKey();

                return;
            }

            Console.WriteLine("list[{0}] = {1}", index, list.Element(index));
            
            Console.WriteLine("Size of the list is {0}", list.Size());
            Console.WriteLine("Press any key to continue.");
            
            Console.ReadKey();
        }

    }
}