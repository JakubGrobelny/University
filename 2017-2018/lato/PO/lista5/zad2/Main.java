// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z2, Wyra¿enia arytmetyczne.
// Program testuj¹cy implementacjê reprezentacji
// wyra¿eñ arytmetycznych.
// Main.java
// 2018-03-29

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.File;
import java.util.ArrayList;
import java.io.FileNotFoundException;
import java.io.IOException;

// Klasa z metod¹ main.
public class Main
{
    public static void main(String[] args) throws FileNotFoundException, IOException
    {
        // Sprawdzanie, czy u¿ytkownik poda³ nazwê pliku do wczytania.
        if (args.length < 1)
        {
            System.out.print("To use the program you need to put the filename");
            System.out.println(" as command line argument!");
            System.out.println("Example:\njava Main file.txt");
            return;
        }
        else
        {
            // Lista napisów, które s¹ podanymi S-wyra¿eniami.
            ArrayList<String> expressions = new ArrayList<String>();
            
            try 
            {
                BufferedReader reader = new BufferedReader(new FileReader(new File(args[0])));
                String line = null;
                
                boolean definitions = true;

                // Wczytywanie kolejnych definicji oraz wyra¿eñ z podanego pliku.
                while ((line = reader.readLine()) != null)
                {
                    // Sprawdzanie, czy aktualna linia jest etykiet¹ „examples”
                    if (line.equals("examples:"))
                        definitions = false;
                    // Je¿eli skoñczono wczytywaæ definicje, to odczytana linia
                    // zostanie dodana do listy „expressions„.
                    else if (!definitions)
                    {
                        if (line.charAt(0) != '#')
                            expressions.add(line);
                    }
                    // W przeciwnym razie dodawany jest nowy element do mapy zmiennych.
                    else if (!line.equals("definitions:") && (line.charAt(0) != '#'))
                    {

                        String[] definition = line.split("\\s+");
                        Variable.addVariable(definition[0], Float.parseFloat(definition[1]));
                    }
                }

                reader.close();
            }
            catch (FileNotFoundException exc)
            {
                System.out.println("Failed to open the file!");
                return;
            }

            try
            {        
                // Ewaluacja kolejnych wyra¿eñ z listy „expressions”.
                for (String expression : expressions)
                {
                    Expression expr = Parser.parseExpression(expression);
                    System.out.print(expr + "=");
                    System.out.println(expr.evaluate() + "\n");
                }
            }
            // Zwracanie wyj¹tku w przypadku napotkania na b³¹d przy ewaluacji.
            catch(Exception exc)
            {
                System.out.println("exception: " + exc);
            }
        }
    }
}   