using System;
using Factory;

namespace Shapes {

    public class Circle : IShape {
        public double Radius {get; set;}
        public double GetArea() {
            return System.Math.PI * Radius * Radius;
        }
 
    }

    public class CircleFactoryWorker : IShapeFactoryWorker {
        public bool AcceptsParameters(string name, object[] parameters) {
            return name == "Circle" 
                && parameters.Length == 1 
                && parameters[0] is double;
        }

        public IShape Create(object[] parameters) {
            return new Circle { Radius = (double)parameters[0]};
        }

    }

    public class Rectangle : IShape {
        public double Width {get; set;}
        public double Height {get; set;}

        public double GetArea() {
            return Width * Height;
        }
    }

    public class RectangleFactoryWorker : IShapeFactoryWorker {
        public bool AcceptsParameters(string name, object[] parameters) {
            return name == "Rectangle" 
                && parameters.Length == 2 
                && parameters[0] is double 
                && parameters[1] is double;
        }

        public IShape Create(object[] parameters) {
            return new Rectangle {
                Width  = (double)parameters[0],
                Height = (double)parameters[1]
            };
        }
    }

    public class LabeledSquare : IShape {
        public double Size {get; set;}
        public string Name {get; set;}

        public double GetArea() {
            return Size * Size;
        }
    }

    public class LabeledSquareFactoryWorker : IShapeFactoryWorker {
        public bool AcceptsParameters(string name, object[] parameters) {
            return name == "Square"
                && parameters.Length == 2
                && parameters[0] is string
                && parameters[1] is double;
        }

        public IShape Create(object[] parameters) {
            return new LabeledSquare {
                Name = (string)parameters[0],
                Size = (double)parameters[1]
            };
        }
    }
}
