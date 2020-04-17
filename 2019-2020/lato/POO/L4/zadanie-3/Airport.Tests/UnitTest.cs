using NUnit.Framework;
using Airport;
using System;

namespace Airport.Tests {
    public class Tests {
        [SetUp]
        public void Setup() {
        }

        [Test]
        public void InvalidCapacity() {
            Assert.Throws<ArgumentException>(
                () => new Airport(-7)
            );

            Assert.Throws<ArgumentException>(
                () => new Airport(0)
            );
        }

        [Test]
        public void ValidCapacity() {
            var airport = new Airport(42);
            Assert.NotNull(airport);
        }

        [Test]
        public void CapacityDepleted() {
            const int capacity = 5;
            var airport = new Airport(capacity);
            for (int i = 0; i < capacity; i++) {
                var plane = airport.AcquirePlane();
                Assert.NotNull(plane);
            }

            Assert.Throws<ArgumentException>(
                () => airport.AcquirePlane()
            );
        }

        [Test]
        public void ReusedPlane() {
            var airport = new Airport(1);
            var plane1 = airport.AcquirePlane();
            airport.ReleasePlane(plane1);
            var plane2 = airport.AcquirePlane();

            Assert.That(plane2, Is.EqualTo(plane1));
        }

        [Test]
        public void InvalidRelease() {
            var airport = new Airport(1);
            var plane1 = airport.AcquirePlane();
            var plane2 = new Plane();

            Assert.Throws<ArgumentException>(
                () => airport.ReleasePlane(plane2)
            );
        }

        [Test]
        public void ReleaseFull() {
            var airport = new Airport(1);
            var plane = new Plane();
            Assert.Throws<ArgumentException>(
                () => airport.ReleasePlane(plane)
            );
        }

        [Test]
        public void TwoAirports() {
            var airport1 = new Airport(1);
            var airport2 = new Airport(2);

            var plane1 = airport1.AcquirePlane();
            var plane2 = airport2.AcquirePlane();

            Assert.Throws<ArgumentException>(
                () => {
                    airport1.ReleasePlane(plane2);
                    airport2.ReleasePlane(plane1);
                }
            );
        }
    }
}