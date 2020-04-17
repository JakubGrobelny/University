using NUnit.Framework;
using System;

namespace Factory.Tests {
    public class Tests {
        [SetUp]
        public void Setup() {
        }

        [Test]
        public void SimpleTest() {
            ShapeFactory factory = new ShapeFactory();
            IShape circle = factory.CreateShape("Circle", 1.0);

            Assert.IsNotNull(circle);
            Assert.That(circle.GetArea(), Is.EqualTo(Math.PI));
        }

        [Test]
        public void InvalidParams() {
            ShapeFactory factory = new ShapeFactory();
            Assert.Throws<ArgumentException>(
                () => factory.CreateShape("Circle", 2.1, 3.7)
            );
        }

        [Test]
        public void InvalidShape() {
            ShapeFactory factory = new ShapeFactory();
            Assert.Throws<ArgumentException>(
                () => factory.CreateShape("FooBar", 1, 2, 3, 7)
            );
        }

        [Test]
        public void TwoShapes() {
            ShapeFactory factory = new ShapeFactory();
            IShape circle = factory.CreateShape("Circle", 2.0);
            IShape rectangle = factory.CreateShape("Rectangle", 3.0, 7.0);

            Assert.IsNotNull(circle);
            Assert.IsNotNull(rectangle);
            Assert.That(rectangle.GetArea(), Is.EqualTo(21.0));
        }

        [Test]
        public void AddWorker() {
            ShapeFactory factory = new ShapeFactory();
            factory.RegisterWorker(new Shapes.LabeledSquareFactoryWorker());

            IShape square = factory.CreateShape("Square", "ABCD", 16.0);
            IShape rectangle = factory.CreateShape("Rectangle", 2.0, 1.0);

            Assert.IsNotNull(square);
            Assert.IsNotNull(rectangle);
            Assert.That(rectangle.GetArea(), Is.EqualTo(2.0));
            Assert.That(square.GetArea(), Is.EqualTo(256));
        }

        [Test]
        public void AddedWorkerWrongArity() {
            ShapeFactory factory = new ShapeFactory();
            factory.RegisterWorker(new Shapes.LabeledSquareFactoryWorker());

            Assert.Throws<ArgumentException>(
                () => factory.CreateShape("Square", "ABCD", 2.0, "invalid")
            );
        }
    }
}