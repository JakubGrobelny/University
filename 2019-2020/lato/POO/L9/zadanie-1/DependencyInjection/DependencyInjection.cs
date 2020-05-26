using System;
using System.Collections.Generic;
using System.Reflection;

namespace Zadanie1 {

    public class SimpleContainer {

        private interface IObjectCreator {
            object CreateObject();
        }

        private class SingletonCreator : IObjectCreator {
            object instance = null;
            ConstructorInfo constructor;

            public SingletonCreator(ConstructorInfo constructor) {
                this.constructor = constructor;
            }

            public object CreateObject() {
                if (this.instance == null) {
                    this.instance = constructor.Invoke(new object[] {});
                }

                return this.instance;
            }            
        }

        private class ObjectCreator : IObjectCreator {
            ConstructorInfo constructor;

            public ObjectCreator(ConstructorInfo constructor) {
                this.constructor = constructor;
            }

            public object CreateObject() => constructor.Invoke(new object[] {});
        }

        private Dictionary<Type, IObjectCreator> instanceCreators;

        public SimpleContainer() {
            this.instanceCreators = new Dictionary<Type, IObjectCreator>();
        }

        private static ConstructorInfo FindConstructor(Type type) {
            return type.GetConstructor(new Type[] {});
        }

        public void RegisterType<T>(bool singleton) where T : class {
            var type = typeof(T);
            var constructor = FindConstructor(type);
            if (singleton) {
                this.instanceCreators[type] = new SingletonCreator(constructor);
            } else {
                this.instanceCreators[type] = new ObjectCreator(constructor);
            }
        }

        public void RegisterType<From, To>(bool singleton) where To : From {
            var from = typeof(From);
            var to = typeof(To);

            var constructor = FindConstructor(to);
            if (singleton) {
                this.instanceCreators[from] = new SingletonCreator(constructor);
            } else {
                this.instanceCreators[from] = new ObjectCreator(constructor);
            }
        }

        private IObjectCreator GetCreator(Type type) {
            IObjectCreator creator;
            return this.instanceCreators.TryGetValue(type, out creator) 
                ? creator 
                : null;
        }

        public T Resolve<T>() where T : class {
            var type = typeof(T);
            var creator = this.GetCreator(type);

            if (creator == null) {
                if (type.IsAbstract || type.IsInterface) {
                    string kind = type.IsAbstract ? "abstract" : "interface";
                    string message = String.Format(
                        "Resolve: Unregistered {1} type {0}", type, kind
                    );
                    throw new UnregisteredInterfaceException(message);
                }

                this.RegisterType<T>(false);
                return this.Resolve<T>();
            }

            return (T)creator.CreateObject();
        }
    }

    [Serializable]
    public class UnregisteredInterfaceException : System.Exception {
        public UnregisteredInterfaceException()
            : base("Unregistered interface type") {}

        public UnregisteredInterfaceException(string message)
            : base(message) {}

        public UnregisteredInterfaceException(string message, Exception inner)
            : base(message, inner) {}
    }
}