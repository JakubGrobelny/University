// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L7, Edytor obiektów.
// Implementacja klasy „Triangle”.
// Triangle.java
// 2018-04-14

import java.io.*;

// Klasa reprezentuj¹ca Trójk¹t.
public class Triangle extends Figure
{
    // Tablice wspó³rzêdnych x i y trzech wierzcho³ków trójk¹ta.
    private double[] x;
    private double[] y;

    // Unikalny identyfikator s³u¿¹cy do serializacji.
    private static final long serialVersionUID = 7524472295622776147L;;

    // Metoda wczytuj¹ca obiekt z pliku o podanej nazwie.
    // Zwraca fa³sz, je¿eli nie uda³o siê odczytaæ danych z podanego pliku.
    public boolean readFromFile(String path)
    {
        try
        {
            FileInputStream file = new FileInputStream(path);
            ObjectInputStream input = new ObjectInputStream(file);

            // Tworzenie tymczasowego obiektu i kopiowanie jego wartoœci.
            Triangle temp = (Triangle)input.readObject();
            this.x = temp.x;
            this.y = temp.y;
            this.posX = temp.posX;
            this.posY = temp.posY;
            this.name = temp.name;

            // Zamykanie pliku.
            input.close();
            file.close();

            return true;
        }
        catch (Exception exc)
        {
            return false;
        }
    }

    // Metoda zapisuj¹ca obiekt do pliku.
    // Zwraca fa³sz, je¿eli nie uda³o siê zapisaæ obiektu.
    public boolean writeToFile(String path)
    {
        try
        {
            FileOutputStream file = new FileOutputStream(path);
            ObjectOutputStream output = new ObjectOutputStream(file);
            
            output.writeObject(this);
            output.close();
            file.close();

            return true;
        }
        catch (Exception exc)
        {
            return false;
        }
    }

    // Metoda zamieni¹ca obiekt na napis, który zawiera informacje
    // o polach obiektu.
    public String toString()
    {
        String vertexPositions = new String();

        // Przepisywanie wspó³rzêdnych wierzcho³ków.
        for (int i = 0; i < 3; i++)
        {
            vertexPositions = vertexPositions + " " +
                              "x" + Integer.toString(i + 1) + ": " +
                              Double.toString(x[i]) + " " +
                              "y" + Integer.toString(i + 1) + ": " +
                              Double.toString(y[i]);
        }

        return name + vertexPositions;
    }

    // Metoda prywatna zwracaj¹ca wspó³rzêdn¹ x lub y wierzcholka o indeksie
    // „which”. Je¿eli jako „x” podana zostanie prawda, to zwrócona zostanie
    // wspó³rzêdna x. W przeciwnym razie zwrócona zostanie wspólrzêdna y.
    private double getVertex(int which, boolean x)
        throws IllegalArgumentException
    {
        if (which > 2 || which < 0)
            throw new IllegalArgumentException("Illegal vertex index!"); 

        return x ? this.x[which] : this.y[which];
    }

    // Metoda zwracaj¹ca wspó³rzêdn¹ x wierzcho³ka o indeksie „which”.
    public double getVertexPosX(int which)
        throws IllegalArgumentException
    {
        return this.getVertex(which, true);
    }

    // Metoda zwracaj¹ca wspó³rzêdn¹ y wierzcho³ka o indeksie „which”.
    public double getVertexPosY(int which)
        throws IllegalArgumentException
    {
        return this.getVertex(which, false);
    }

    // Metoda ustawiaj¹ca wspó³rzêdne wierzcho³ka o indeksie „which” na
    // podane wartoœci.
    public void setVertex(int which, double x, double y)
        throws IllegalArgumentException
    {
        if (which > 2 || which < 0)
            throw new IllegalArgumentException("Illegal vertex index!");

        this.x[which] = x;
        this.y[which] = y;

        // Obliczanie nowego œrodka trójk¹ta.
        double sumX = 0;
        for (double posX : this.x)
            sumX += posX;
        
        double sumY = 0;
        for (double posY : this.y)
            sumY += posY;

        this.posX = sumX / 3;
        this.posY = sumY / 3;
    }

    // Konstruktor.
    public Triangle(double x1, double x2, double x3,
                    double y1, double y2, double y3, String name)
    {
        super((x1 + x2 + x3) / 3,
              (y1 + y2 + y3) / 3,
              name);

        this.x = new double[3];
        this.y = new double[3];

        this.x[0] = x1;
        this.x[1] = x2;
        this.x[2] = x3;

        this.y[0] = y1;
        this.y[1] = y2;
        this.y[2] = y3;
    }

    // Konstruktor bezargumentowy.
    public Triangle()
    {
        this(0, 1, 0, 1, 0, 0, "");
    }    
}