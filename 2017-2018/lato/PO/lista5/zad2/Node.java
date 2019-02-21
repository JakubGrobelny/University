// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L5, z2, Wyra¿enia arytmetyczne.
// Implementacja reprezentacji wyra¿eñ
// arytmetycznych w postaci drzew.
// Node.java
// 2018-03-29

// Klasa reprezentuj¹ca wêze³ drzewa zawieraj¹cy operator binarny
// oraz wskaŸniki do prawego i lewego podwyra¿enia.
public class Node extends Expression
{
    // Prywatne pola przechowuj¹ce operator 
    // binarny oraz lewe i prawe podwyra¿enia.
    private char operator;
    private Expression left;
    private Expression right;

    // Metoda wyliczaj¹ca wartoœæ wyra¿enia. 
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

    // Zamiana drzewa na ³añcuch znaków.
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
