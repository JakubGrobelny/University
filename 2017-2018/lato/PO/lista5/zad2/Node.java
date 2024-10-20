// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z2, Wyrażenia arytmetyczne.
// Implementacja reprezentacji wyrażeń
// arytmetycznych w postaci drzew.
// Node.java
// 2018-03-29

// Klasa reprezentująca węzeł drzewa zawierający operator binarny
// oraz wskaźniki do prawego i lewego podwyrażenia.
public class Node extends Expression
{
    // Prywatne pola przechowujące operator 
    // binarny oraz lewe i prawe podwyrażenia.
    private char operator;
    private Expression left;
    private Expression right;

    // Metoda wyliczająca wartość wyrażenia. 
    public float evaluate() throws IllegalStateException
    {
        switch (operator)
        {
            case '+':
                return left.evaluate() + right.evaluate();
            case '-':
                return left.evaluate() - right.evaluate();
            case '*':
                return left.evaluate() * right.evaluate();
            case '/':
                // Zabezpieczenie przed dzieleniem przez zero.
                float rightExpr = right.evaluate();
                if (rightExpr == 0)
                    throw new IllegalStateException("evaluate(): division by 0!");
                else
                    return left.evaluate() / rightExpr;
            default:
                throw new IllegalStateException();
        }
    }

    // Zamiana drzewa na łańcuch znaków.
    public String toString()
    {
        return  "(" + left.toString() + String.valueOf(operator) + right.toString() + ")";
    }

    // Konstruktor
    public Node(char operator, Expression left, Expression right)
    {
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}
