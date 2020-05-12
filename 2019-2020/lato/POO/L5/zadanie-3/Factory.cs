using System.Collections.Generic;
using System;

namespace Factory {

    public interface IShapeFactory {
        void RegisterWorker(IShapeFactoryWorker worker);
        IShape CreateShape(string name, params object[] parameters);

    }

    public interface IShapeFactoryWorker {        
        bool AcceptsParameters(string name, object[] parameters);

        IShape Create(object[] parameters);
    }

    public interface IShape {
        double GetArea();
    }

    public class ShapeFactory : IShapeFactory {
        List<IShapeFactoryWorker> workers = new List<IShapeFactoryWorker>();

        public ShapeFactory() {
            this.RegisterWorker(new CircleFactoryWorker());
        }

        public void RegisterWorker(IShapeFactoryWorker worker) {
            this.workers.Add(worker);
        }

        public IShape CreateShape(string name, params object[] parameters) {
            foreach (var worker in workers) {
                if (worker.AcceptsParameters(name, parameters)) {
                    return worker.Create(parameters);
                }
            }

            throw new ArgumentException(
                String.Format("Unrecognized shape name '{0}'", name)
            );
        }
    }

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
}