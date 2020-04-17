using System;
using System.Collections.Generic;

namespace Factory {

    public interface IShapeFactoryWorker {        
        bool AcceptsParameters(string name, object[] parameters);

        IShape Create(object[] parameters);
    }

    public interface IShape {
        double GetArea();
    }

    public class ShapeFactory {
        List<IShapeFactoryWorker> workers = new List<IShapeFactoryWorker>();

        public ShapeFactory() {
            this.RegisterWorker(new Shapes.CircleFactoryWorker());
            this.RegisterWorker(new Shapes.RectangleFactoryWorker());
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
}

