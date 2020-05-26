using NUnit.Framework;
using Zadanie1;

namespace DependencyInjection.Tests {

    public class Tests {
        [SetUp]
        public void Setup() {}

        class Foo {}

        [Test]
        public void TestSingleton() {
           var container = new SimpleContainer();
           container.RegisterType<Foo>(true);

           var foo1 = container.Resolve<Foo>();
           var foo2 = container.Resolve<Foo>();

            Assert.That(foo1, Is.EqualTo(foo2));
        }

        [Test]
        public void TestDifferentInstances() {
            var container = new SimpleContainer();
            container.RegisterType<Foo>(false);

            var foo1 = container.Resolve<Foo>();
            var foo2 = container.Resolve<Foo>();

            Assert.That(foo1, Is.Not.EqualTo(foo2));
        }

        [Test]
        public void TestUnregistered() {
            var container = new SimpleContainer();
            var instance = container.Resolve<Foo>();

            Assert.NotNull(instance);
            Assert.IsInstanceOf(typeof(Foo), instance);
        }

        abstract class AbstractBar {}
        class Bar1 : AbstractBar {}
        class Bar2 : AbstractBar {}

        [Test]
        public void TestUnregisteredAbstract() {
            var container = new SimpleContainer();

            Assert.Throws<UnregisteredInterfaceException>(
                () => container.Resolve<AbstractBar>()
            );
        }

        [Test]
        public void TestAbstractSingleton() {
            var container = new SimpleContainer();
            container.RegisterType<AbstractBar, Bar1>(true);
        
            var bar1 = container.Resolve<AbstractBar>();
            var bar2 = container.Resolve<AbstractBar>();

            Assert.IsInstanceOf(typeof(Bar1), bar1);
            Assert.IsInstanceOf(typeof(Bar1), bar2);
            Assert.That(bar1, Is.EqualTo(bar2));
        }

        [Test]
        public void TestAbstract() {
            var container = new SimpleContainer();
            container.RegisterType<AbstractBar, Bar2>(false);

            var instance = container.Resolve<AbstractBar>();
            Assert.IsInstanceOf(typeof(Bar2), instance);
        }

        interface IBar {}
        class Bar3 : IBar {}
        class Bar4 : IBar {}

        [Test]
        public void TestInterfaceSingleton() {
            var container = new SimpleContainer();
            container.RegisterType<IBar, Bar3>(true);
        
            var bar1 = container.Resolve<IBar>();
            var bar2 = container.Resolve<IBar>();

            Assert.IsInstanceOf(typeof(Bar3), bar1);
            Assert.IsInstanceOf(typeof(Bar3), bar2);
            Assert.That(bar1, Is.EqualTo(bar2));
        }

        [Test]
        public void TestInterface() {
            var container = new SimpleContainer();
            container.RegisterType<IBar, Bar4>(false);
            var instance = container.Resolve<IBar>();

            Assert.IsInstanceOf(typeof(Bar4), instance);
        }

        [Test]
        public void TestChangeAbstractType() {
            var container = new SimpleContainer();

            container.RegisterType<AbstractBar, Bar1>(false);

            var bar1 = container.Resolve<AbstractBar>();

            container.RegisterType<AbstractBar, Bar2>(false);
            
            var bar2 = container.Resolve<AbstractBar>();

            Assert.IsInstanceOf(typeof(Bar1), bar1);
            Assert.IsInstanceOf(typeof(Bar2), bar2);
        }

        [Test]
        public void TestChangeInterfaceType() {
            var container = new SimpleContainer();
            container.RegisterType<IBar, Bar3>(false);

            var bar1 = container.Resolve<IBar>();

            container.RegisterType<IBar, Bar4>(false);
            
            var bar2 = container.Resolve<IBar>();

            Assert.IsInstanceOf(typeof(Bar3), bar1);
            Assert.IsInstanceOf(typeof(Bar4), bar2);
        }
    }
}