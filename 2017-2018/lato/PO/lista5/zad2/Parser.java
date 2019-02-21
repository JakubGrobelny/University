// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z2, Wyra¿enia arytmetyczne.
// Klasa parsuj¹ca S-wyra¿enia do drzew.
// Parser.java
// 2018-03-29

import java.lang.StringBuilder;

// Klasa zawieraj¹ca metody statyczne s³u¿¹ce do przekszta³cania
// S-wyra¿eñ z operatorami binarnymi na drzewa reprezentowane przez
// klasê Expression.
public class Parser
{
    // Metoda sprawdzaj¹ca czy napis reprezentuje liczbê typu float.
    private static boolean isNumber(String str)
    {
        try
        {
            Float.parseFloat(str);
            return true;
        }
        catch(NumberFormatException exc)
        {
            return false;
        }
        
    }

    // Metoda sprawdzaj¹ca, czy wyra¿enie jest list¹.
    private static boolean isList(String str) throws IllegalArgumentException
    {
        if (str.length() < 2)
            return false;
        else if (str.charAt(0) == '(' && str.charAt(str.length() - 1) == ')')
        {
            // Poprawne wyra¿enie powinno zawieraæ operator oraz dwa
            // podwyra¿enia. np.: (+ 1 1). Spacje równie¿ musz¹ siê zgadzaæ.
            if ((str.length() < 7) || (str.charAt(2) != ' '))
                throw new IllegalArgumentException("isList(): " + str + " invalid expression!");
            else
                return true;
        }
        else
            return false;
    }

    // Metoda zwracaj¹ca operator z S-wyra¿enia.
    private static char getOperator(String list)
    {
        // Metoda ta jest wywo³ywana tylko, gdy „list” jest poprawn¹ list¹,
        // czyli mo¿emy za³o¿yæ, ¿e jest co najmniej d³ugoœci 7.
        return list.charAt(1);
    }

    // Metoda zwracaj¹ca pierwsze podwyra¿enie S-wyra¿enia
    private static String getFirst(String list) throws IllegalArgumentException
    {
        int bracketCounter = 0;
        StringBuilder result = new StringBuilder();

        for (int i = 2; i < list.length(); i++)
        {
            if (list.charAt(i) == '(')
            {
                bracketCounter++;
                result.append(list.charAt(i));
            }
            else if (list.charAt(i) == ')')
            {   
                bracketCounter--;
                result.append(list.charAt(i));

                if (bracketCounter == 0)
                    return result.toString();
            }
            else if ((list.charAt(i) == ' ') && bracketCounter == 0)
            {
                if (!result.toString().equals(""))
                    return result.toString();
            }
            else
                result.append(list.charAt(i));
        }

        throw new IllegalArgumentException("getFirst():" + list + " invalid expression!");
    }

    // Metoda zwracaj¹ca drugie podwyra¿enie S-wyra¿enia
    private static String getSecond(String list)
    {
        int bracketCounter = 0;
        StringBuilder result = new StringBuilder();

        for (int i = list.length() - 2; i >= 0; i--)
        {
            if (list.charAt(i) == ')')
            {
                bracketCounter++;
                result.append(list.charAt(i));
            }
            else if (list.charAt(i) == '(')
            {
                bracketCounter--;
                result.append(list.charAt(i));

                if (bracketCounter == 0)
                    return result.reverse().toString();
            }
            else if ((list.charAt(i) == ' ') && bracketCounter == 0)
            {
                if (!result.toString().equals(""))
                    return result.reverse().toString();
            }
            else
                result.append(list.charAt(i));
        }

        throw new IllegalArgumentException("getSecond():" + list + " invalid expression!");
    }

    // Funkcja zamieniaj¹ca S-wyra¿enia na drzewa
    public static Expression parseExpression(String expr)
    {
        if (isList(expr))
        {
            char operator = getOperator(expr);
            String left = getFirst(expr);
            String right = getSecond(expr);
            
            return new Node(operator, parseExpression(left), parseExpression(right));
        }
        else if (isNumber(expr))
            return new Constant(Float.parseFloat(expr));
        else
            return new Variable(expr);
    }
}