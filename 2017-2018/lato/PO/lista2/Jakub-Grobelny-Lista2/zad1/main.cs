// Jakub Grobelny 300481
// 7 marca 2018
// Programowanie Obiektowe - Lista 2, zadanie 1

// Program generuje pewną ilość losowych łańcuchów znaków o długości
// odpowiadającej kolejnym liczbom pierwszym wykorzystując strumienie
// zdefiniowane w pliku streams.cs

using System;
using Streams;

class MainClass
{
    public static void Main()
    {
        var primeStream = new PrimeStream();
        var intStream = new IntStream();
        var stringStream = new RandomWordStream();

        Console.WriteLine("How many strings do you want to generate?");
        Console.WriteLine("(Enter a number smaller than 1 to generate the entire stream)");
        var input = Console.ReadLine();
        int amount;

        if (!int.TryParse(input, out amount))
        {
            Console.WriteLine("Invalid input!");
            return;
        }

        if (amount <= 0)
        {
            while (!stringStream.Eos())
            {
                Console.WriteLine("{0}) {1} characters: {2}", intStream.Next(), primeStream.Next(), stringStream.Next());                
            }
        }
        else
        {
            for (int i = intStream.Next(); i < amount; i++)
            {
                Console.WriteLine("{0}) {1} characters: {2}", intStream.Next(), primeStream.Next(), stringStream.Next());
            }
        }
    }
}
