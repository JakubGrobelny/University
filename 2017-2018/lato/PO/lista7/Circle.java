import java.io.*;

public class Circle extends Figure
{
    private double radius;

    private static final long serialVersionUID = 7326472295622776147L;

    public boolean readFromFile(String path)
    {
        try
        {
            FileInputStream file = new FileInputStream(path);
            ObjectInputStream input = new ObjectInputStream(file);
            
            Circle temp = (Circle)input.readObject();
            
            input.close();
            file.close();
            
            this.radius = temp.radius;
            this.posX = temp.posX;
            this.posY = temp.posY;
            this.name = temp.name;

            return true;
        }
        catch (Exception exc)
        {
            return false;
        }
    }

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

    public String toString()
    {
        return super.toString() + 
               " r: " + Double.toString(radius);
    }

    public void setRadius(double r)
    {
        this.radius = r;
    }

    public double getRadius()
    {
        return this.radius;
    }

    public Circle(double x, double y, double r, String name)
    {
        super(x, y, name);
        this.radius = r;
    }

    public Circle()
    {
        this(0, 0, 1, "");
    }
}