// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z1, Kolekcje porównywalnych elementów.
// Program testuj¹cy implementacjê kolekcji sortowanych rosn¹co.
// Main.java
// 2018-03-30

import java.util.Random;

public class Main
{
    // Metoda main
    public static void main(String[] args) 
    {
        // Lista losowo wygenerowanych funkcji.
        AscendingList<Function> funList = generateTests();
        
        // Wypisywanie i usuwanie kolejnych funkcji z listy.
        while(!funList.empty())
            System.out.println(funList.pop() + "\n");
    }

    // Funkcja generuj¹ca 100 losowych funkcji.
    private static AscendingList<Function> generateTests()
    {
        AscendingList<Function> result = new AscendingList<Function>();

        Random randomizer = new Random();

        for (int i = 0; i < 100; i++)
        {
            int type = randomizer.nextInt() % 3;
            
            if (type < 0)
                type *= -1;

            // Funkcja kwadratowa
            if (type == 0)
            {
                float a = randomizer.nextFloat() * 10;
                float b = randomizer.nextFloat() * 10;
                float c = randomizer.nextFloat() * 10;

                result.push(new Quadratic(a, b, c));
            }
            // Wielomian
            else if (type == 1)
                result.push(randomPolynomial());
            // Funkcja Wymierna
            else if (type == 2)
                result.push(new Rational(randomPolynomial(), randomPolynomial()));
        }

        return result;
    }

    private static Polynomial randomPolynomial()
    {
        Random randomizer = new Random();
        
        int coeffCount = randomizer.nextInt() % 9;

        if (coeffCount < 0)
            coeffCount *= -1;
        coeffCount++;

        float[] coeffs = new float[coeffCount];

        for (int i = 0; i < coeffCount; i++)
            coeffs[i] = randomizer.nextFloat() * 10;

        return new Polynomial(coeffs);
    }
}