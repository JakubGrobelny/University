using System;
using System.Collections.Generic;

// wstrzykiwana implementacja: PersonRegistry

namespace Zadanie5b {
    public class Person { }

    public abstract class PersonNotifier {
        private PersonRegistry registry;

        public PersonNotifier(PersonRegistry registry) {
            this.registry = registry;
        }

        public IEnumerable<Person> GetPersons() {
            return this.registry.GetPersons();
        }

        public abstract void Notify();
    }

    public abstract class PersonRegistry {
        public abstract IEnumerable<Person> GetPersons();
    }

    public class XmlPersonRegistry : PersonRegistry {
        public override IEnumerable<Person> GetPersons() {
            throw new NotImplementedException();
        }
    }

    public class DbPersonRegistry : PersonRegistry {
        public override IEnumerable<Person> GetPersons() {
            throw new NotImplementedException();
        }
    }

    public class MailPersonNotifier : PersonNotifier {
        public MailPersonNotifier(PersonRegistry registry) : base(registry) {}
        
        public override void Notify() {
            foreach (var person in this.GetPersons()) {
                throw new NotImplementedException();
            }
        }
    }

    public class SmsPersonNotifier : PersonNotifier {
        public SmsPersonNotifier(PersonRegistry registry) : base(registry) {}

        public override void Notify() {
            foreach (var person in this.GetPersons()) {
                throw new NotImplementedException();
            }
        }
    }
}
