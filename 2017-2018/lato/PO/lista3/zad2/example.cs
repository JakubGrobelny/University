// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L3, z2, S³ownik
// Program testuj¹cy implementacjê s³owników.
// example.cs
// 2018-03-14

using System;
using Dictionaries;

class MainClass
{
    public static void Main()
    {
        // Przyk³adowy s³ownik liczb zmiennoprzecinkowych indeksowany wyrazami.
        Dictionary<string, float> dict = new Dictionary<string, float>();

        // Wypisywanie instrukcji.
        PrintInstructions();

        bool exit = false;
        Console.Write("$ ");

        // Wczytywanie poleceñ dopóty, dopóki u¿ytkownik nie u¿yje
        // komendy 'exit'.
        while (!exit)
        {
            string input = Console.ReadLine();
            ParseInput(input, ref dict, ref exit);
        }
    }

    // Przetwarzanie otrzymanej komendy.
    private static void ParseInput(string input, ref Dictionary<string, float> dict, ref bool exit)
    {
        string[] tokens = input.Split(' ');

        if (input.Length == 0)
            return;

        switch (tokens[0])
        {
            // Wyjœcie.
            case "exit":

                exit = true;
                break;

            // Usuniêcie elementu.
            case "delete":

                if (tokens.Length < 2)
                {
                    Console.Write("Invalid input! \n$ ");
                    break;
                }

                dict.Remove(tokens[1]);
                Console.Write("$ ");

                break;

            // Sprawdzenie wartoœci dla zadanego klucza.
            case "see":

                try
                {
                    Console.Write("dict[\"{0}\"] = {1} \n$ ", tokens[1], dict[tokens[1]]);
                }
                catch
                {
                    Console.Write("Invalid key! \n$ ");
                    break;
                }

                break;

            // Wypisywanie zawartoœci s³ownika.
            case "print":

                for (int i = 0; i < dict.GetSize(); i++)
                {
                    string temp = dict.GetKey(i);
                    Console.WriteLine("dict[\"{0}\"] = {1}", temp, dict[temp]);
                }
                Console.Write("$ ");
                break;

            // Ustawianie elementu pod zadanym indeksem na zadan¹ wartoœæ.
            case "set":

                if (tokens.Length < 3)
                {
                    Console.Write("Invalid input! \n$ ");
                    break;
                }

                try
                {
                    float val= float.Parse(tokens[2],
                                System.Globalization.CultureInfo.InvariantCulture);
                    dict[tokens[1]] = val; 
                    Console.Write("$ ");
                }
                catch
                {
                    Console.Write("Invalid unput! \n$ ");
                }

                                  
                break;

            // Czyszczenie ekranu.
            case "clear":

                Console.Clear();
                Console.Write("$ ");
                break;

            // B³¹d.
            default:

                Console.Write("Invalid input! \n$ ");
                break;
        }
    }

    // Wypisywanie pocz¹tkowych instrukcji na ekranie
    private static void PrintInstructions()
    {
        Console.WriteLine("Available commands (go to instruction for more details): ");
        Console.WriteLine("set <key> <value>");
        Console.WriteLine("see <key>");
        Console.WriteLine("delete <key>");
        Console.WriteLine("print");
        Console.WriteLine("clear");
        Console.WriteLine("exit\n");
    }
}