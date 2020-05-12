using System;
using ObjPool;
using Factory;

namespace Zadanie3 {

    public class AirportProxy : IAirport {
        Airport airport;

        private bool IsClosed() {
            var time = DateTime.Now;
            bool tooLate = time.Hour > 22
                        || (time.Hour == 22 && time.Minute != 0);

            return time.Hour < 8 || tooLate;
        }

        public AirportProxy(int capacity) {
            this.airport = new Airport(capacity);
        }
    

        public Plane AcquirePlane() {
            if (IsClosed()) {
                throw new InvalidOperationException();
            }

            return this.airport.AcquirePlane();
        }

        public void ReleasePlane(Plane plane) {
            if (IsClosed()) {
                throw new InvalidOperationException();
            }

            this.airport.ReleasePlane(plane);
        }
    }

    public class FactoryProxy : IShapeFactory {
        ShapeFactory factory;

        public FactoryProxy() {
            this.factory = new ShapeFactory();
        }

        private void LogTime() {
            Console.Write("{0}: ", DateTime.Now);
        }
        
        public void RegisterWorker(IShapeFactoryWorker worker) {
            LogTime();
            Console.WriteLine("RegisterWorker({0})", worker);

            this.factory.RegisterWorker(worker);

            LogTime();
            Console.WriteLine("result = void");
        }
        
        public IShape CreateShape(string name, params object[] parameters) {
            LogTime();
            Console.WriteLine("CreateShape(\"{0}\", {1})", name, parameters);

            var result = this.factory.CreateShape(name, parameters);

            LogTime();
            Console.WriteLine("result = {0}", result);
            return result;
        }
    }

    class Examples {
        public static void Main() {
            try {
                var proxy = new AirportProxy(5);
                var plane = proxy.AcquirePlane();
                proxy.ReleasePlane(plane);
                Console.WriteLine("Airport used successfully.");
            } catch (Exception) {
                Console.WriteLine("Airport was closed.");
            }

            var factory = new FactoryProxy();
            factory.RegisterWorker(new CircleFactoryWorker());
            factory.CreateShape("Circle", 3.14);
        }
    }
}











