using System;
using System.Collections.Generic;

// wstrzykiwana implementacja: PersonNotifier

namespace Zadanie5a {

    public class Person { }

    public abstract class PersonRegistry {
        PersonNotifier notifier;

        public PersonRegistry(PersonNotifier notifier) {
            this.notifier = notifier;
        }

        public abstract IEnumerable<Person> GetPersons();

        public void Notify() {
            this.notifier.Notify(this.GetPersons());
        }
    }

    public abstract class PersonNotifier {
        public abstract void Notify(IEnumerable<Person> persons);
    }

    public class MailPersonNotifier : PersonNotifier {
        public override void Notify(IEnumerable<Person> persons) {
            throw new NotImplementedException();
        }
    }

    public class SmsPersonNotifier : PersonNotifier {
        public override void Notify(IEnumerable<Person> persons) {
            throw new NotImplementedException();
        }
    }

    public class XmlPersonRegistry : PersonRegistry {
        public XmlPersonRegistry(PersonNotifier notifier) : base(notifier) {}

        public override IEnumerable<Person> GetPersons() {
            throw new NotImplementedException();
        }
    }

    public class DbPersonRegistry : PersonRegistry {
        public DbPersonRegistry(PersonNotifier notifier) : base (notifier) {}

        public override IEnumerable<Person> GetPersons() {
            throw new NotImplementedException();
        }
    }
}
