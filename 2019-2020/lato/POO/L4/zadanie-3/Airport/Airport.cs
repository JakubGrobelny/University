using System;
using System.Collections.Generic;
using System.Linq;

namespace Airport {

    public class Plane {
    }

    public class Airport {
        private int capacity;

        private List<Plane> avaliable = new List<Plane>();
        private List<Plane> issued    = new List<Plane>();

        public Airport(int capacity) {
            if (capacity <= 0) {
                throw new ArgumentException();
            }

            this.capacity = capacity;
        }

        public Plane AcquirePlane() {
            if (issued.Count >= capacity) {
                throw new ArgumentException();
            }

            if (avaliable.Count() == 0) {
                avaliable.Add(new Plane());
            }

            var plane = avaliable[0];
            avaliable.Remove(plane);
            issued.Add(plane);

            return plane;
        }

        public void ReleasePlane(Plane plane) {
            if (!issued.Contains(plane)) {
                throw new ArgumentException();
            }

            issued.Remove(plane);
            avaliable.Add(plane);
        }
    }
}
