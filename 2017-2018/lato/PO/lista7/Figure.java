// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L7, Edytor obiektów.
// Abstrakcyjna klasa „Figure”.
// Figure.java
// 2018-04-14

import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

// Abstrakcyjna klasa „Figure” implementuj¹ca interfejs „Serializable”
// pozwalaj¹cy  na zapisywanie i odczytywanie obiektów z pliku.
public abstract class Figure implements Serializable
{
    // Nazwa oraz wspó³rzêdne œrodka figury.
    protected String name;
    protected double posX;
    protected double posY;

    // Unikalny identyfikator s³u¿¹cy do serializacji.
    private static final long serialVersionUID = 7521472295622776147L;;

    // Abstrakcyjne metody zapisu/odczytu obiektu z pliku.
    public abstract boolean readFromFile(String path);    
    public abstract boolean writeToFile(String path);

    // Metoda zamieniaj¹ca figurê na napis.
    public String toString()
    {
        return name + " " + 
               "x: " + Double.toString(posX) + ", " + 
               "y: " + Double.toString(posY);
    }

    // Settery/gettery pól obiektu.

    public void setName(String name)
    {
        this.name = name;
    }

    public void setPosX(double x)
    {
        this.posX = x;
    }

    public void setPosY(double y)
    {
        this.posY = y;
    }

    public String getName()
    {
        return this.name;
    }

    public double getPosX()
    {
        return this.posX;
    }

    public double getPosY()
    {
        return this.posY;
    }

    // Konstruktor.
    public Figure(double x, double y, String name)
    {
        this.posX = x;
        this.posY = y;
        this.name = name;
    }

    // Konstruktor bezargumentowy.
    public Figure()
    {
        this(0, 0, "");
    }
}