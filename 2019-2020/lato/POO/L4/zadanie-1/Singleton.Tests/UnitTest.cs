using NUnit.Framework;
using System;
using Singleton;
using System.Threading;

namespace Singleton.Tests {

    public class Tests {

        [SetUp]
        public void Setup() {
        }

        [Test]
        public void OneInstance() {
            var s1 = GlobalSingleton.Instance();
            var s2 = GlobalSingleton.Instance();

            Assert.That(s1, Is.EqualTo(s2));
        }

        [Test]
        public void OneGlobalInstance() {
            GlobalSingleton s1 = null;
            GlobalSingleton s2 = null;

            var thread1 = new Thread(() => s1 = GlobalSingleton.Instance());
            var thread2 = new Thread(() => s2 = GlobalSingleton.Instance());

            thread1.Start();
            thread2.Start();

            thread1.Join();
            thread2.Join();

            Assert.That(s1, Is.EqualTo(s2));
        }

        [Test]
        public void TheadSingletonNotNull() {
            var s = ThreadSingleton.Instance();
            Assert.NotNull(s);
        }

        [Test]
        public void UniqueInstancesPerThead() {
            ThreadSingleton s1 = null;
            ThreadSingleton s2 = null;

            var thread1 = new Thread(() => s1 = ThreadSingleton.Instance());
            var thread2 = new Thread(() => s2 = ThreadSingleton.Instance());

            thread1.Start();
            thread2.Start();

            thread1.Join();
            thread2.Join();

            Assert.That(s1, Is.Not.EqualTo(s2));

        }

        [Test]
        public void SingleInstancePerThead() {
            Assert.That(
                ThreadSingleton.Instance(), 
                Is.EqualTo(ThreadSingleton.Instance())
            );
        }

        [Test]
        public void TimedSingletonNotNull() {
            var s = TimedSingleton.Instance();
            Assert.NotNull(s);
        }

        [Test]
        public void SingleTimedInstance() {
            var s1 = TimedSingleton.Instance();
            var s2 = TimedSingleton.Instance();

            Assert.That(s1, Is.EqualTo(s2));
        }

        [Test]
        public void InstanceResets() {
            var s1 = TimedSingleton.Instance();
            Thread.Sleep(6000);
            var s2 = TimedSingleton.Instance();

            Assert.That(s1, Is.Not.EqualTo(s2));
        }
    }
}