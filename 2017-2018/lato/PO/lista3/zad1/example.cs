// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L3, z1, Listy dwukierunkowe
// Program testuj¹cy implementacjê list
// example.cs
// 2018-03-14

using System;
using Lists;

class MainClass
{
    public static void Main()
    {
        var list = new List<int>();

        bool exit = false;

        while (!exit)
        {
            PrintMenu();
            Console.Write("Current contents of the list: ");
            PrintList(list);

            // Wczytywanie opcji
            var choice = Console.ReadKey(true).KeyChar;

            switch (choice)
            {
                // Dodawanie elementu na pocz¹tek.
                case '1':
                {
                    Console.Write("Enter the number that you want to add: ");

                    int element;
                    if (!int.TryParse(Console.ReadLine(), out element))
                        Console.WriteLine("Invalid input!");
                    else
                        list.PushFront(element);
                    break;
                }
                // Dodawanie elementu na koniec.
                case '2':
                {
                    Console.Write("Enter the number that you want to add: ");

                    int element;
                    if (!int.TryParse(Console.ReadLine(), out element))
                        Console.WriteLine("Invalid input!");
                    else
                        list.PushBack(element);
                    break;
                }
                // Usuwanie elementu z pocz¹tku.
                case '3':
                    if (list.IsEmpty())
                        Console.WriteLine("Can't remove element from empty list");
                    else
                        Console.WriteLine("First element: {0}", list.PopFront());
                    Console.WriteLine("Press any key to continue.");
                    Console.ReadKey();
                    break;
                // Usuwanie elementu z koñca.
                case '4':
                    if (list.IsEmpty())
                        Console.WriteLine("Can't remove element from empty list");
                    else
                        Console.WriteLine("Last element: {0}", list.PopBack());
                    Console.WriteLine("Press any key to continue.");
                    Console.ReadKey();
                    break;
                // Wypisywanie i-tego elementu.
                case '5':
                {
                    Console.Write("Enter the index: ");

                    int index;
                    if (!int.TryParse(Console.ReadLine(), out index))
                        Console.WriteLine("Invalid input!");
                    else if (index >= 0 && index < list.Size())
                        Console.WriteLine("list[{0}] = {1}", index, list[index]);
                    else
                        Console.WriteLine("Invalid index!");

                    Console.WriteLine("Press any key to continue.");
                    Console.ReadKey();
                    break;
                }
                // Wychodzenie z programu.
                case '6':
                    exit = true;
                    break;
                // B³êdne wejœcie.
                default:
                    Console.WriteLine("There is no such option!");
                    Console.WriteLine("Press any key.");
                    Console.ReadKey();
                    break;
            }

            Console.Clear();
        }

    }

    // Procedura wypisuj¹ca menu g³ówne.
    private static void PrintMenu()
    {
        Console.WriteLine("-------------------------------------");
        Console.WriteLine("Enter the number corresponding to    ");
        Console.WriteLine("the action you want to perform:      ");
        Console.WriteLine("-------------------------------------");
        Console.WriteLine("1) Add new element at the beginning  ");
        Console.WriteLine("2) Add new element at the end        ");
        Console.WriteLine("3) Remove and print the first element");        
        Console.WriteLine("4) Remove and print the last element ");
        Console.WriteLine("5) Print i-th element                ");
        Console.WriteLine("6) Exit                             \n");
    }

    // Procedura wypisuj¹ca zawartoœæ listy.
    private static void PrintList<T>(List<T> list)
    {
        Console.Write("(");
        for (int i = 0; i < list.Size(); i++)
            Console.Write(" {0} ", list[i]);
        Console.WriteLine(")");
    }
}