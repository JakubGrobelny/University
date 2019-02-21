// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L6, z2, Kolekcja implementuj¹ca interfejs „Collection”.
// Test implementacji kolekcji.
// Main.java
// 2018-04-05

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.File;

public class Main
{
    public static void main(String[] args)
    {
        // Sprawdzanie, czy u¿ytkownik poda³ nazwê pliku.
        if (args.length != 1)
        {
            System.out.print("To use the program you need to put the filename");
            System.out.println(" as command line argument!");
            System.out.println("Example:\njava Main file.txt");
            return;
        }

        // Tymczasowa lista przechowuj¹ca kolejne linie z pliku.
        List<String> fileContents = new List<String>();

        try 
        {
            // Wczytywanie zawartoœci pliku.
            BufferedReader reader = new BufferedReader(new FileReader (new File(args[0])));
            String line = null;
            
            while ((line = reader.readLine()) != null)
                fileContents.pushBack(line);

            reader.close();
        } 
        catch (Exception exc) 
        {
            System.out.println(exc);
            return;
        }

        // Lista przechowuj¹ca liczby typu float, które zosta³y wczytane z pliku.
        List<Float> floatList = new List<Float>();

        // Pêtla „for («typ» «element» : «kolekcja»)” mo¿e zostaæ u¿yta
        // dziêki temu, ¿e typ „List” implementuje interfejs „Collection”.

        // Przepisywanie kolejnych elementów do listy liczb.
        for (String line : fileContents)
            floatList.add(Float.parseFloat(line));
        
        // Wypisywanie zawaratoœci listy liczb.
        System.out.println("List: ");
        for (Float number : floatList)
            System.out.print(String.format("%f, ", number));
        System.out.println();
    }
}