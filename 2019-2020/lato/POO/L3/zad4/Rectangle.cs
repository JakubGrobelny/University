// Poprawione:

using System;

public abstract class Shape {
    public abstract int GetArea();
}

public class Rectangle : Shape {
    public virtual int Width {get; set;}
    public virtual int Height {get; set;}

    public override int GetArea() {
        return Width * Height;
    }
}

public class Square : Shape {
    public int Size {get; set;}

    public override int GetArea() {
        return Size * Size;
    }
}

// Kod kliencki

public class AreaCalculator {
    public int CalculateArea(Shape shape) {
        return shape.GetArea();
    }
}

public class Example {
    public static void Main() {
        int w = 4, h = 5, size = 7;
        Shape rect = new Rectangle() { Width = w, Height = h};
        Shape square = new Square() { Size = size };

        AreaCalculator calc = new AreaCalculator();

        Console.WriteLine(
            "Prostokąt o wymiarach {0} na {1} ma pole {2}",
            w, h, calc.CalculateArea(rect)
        );

        Console.WriteLine(
            "Kwadrat o wymiarach {0} na {0} ma pole {1}",
            size, calc.CalculateArea(square)
        );
    }
}
