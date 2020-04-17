using System;

namespace Singleton {

    public sealed class GlobalSingleton {
        static GlobalSingleton instance;
        static object lockObj = new object();

        // prywatny konstruktor
        GlobalSingleton() { }

        public static GlobalSingleton Instance() {
            if (instance == null) {
                lock (lockObj) {
                    if (instance == null) {
                        instance = new GlobalSingleton();
                    }
                }
            }
            return instance;
        }
    }

    public sealed class ThreadSingleton {
        // każdy wątek ma własną statyczną instancję obiektu
        [ThreadStatic] static ThreadSingleton instance;

        ThreadSingleton() {}

        public static ThreadSingleton Instance() {
            // nie potrzebujemy blokady, bo każdy wątek powinien
            // utworzyć swoją własną kopię
            if (instance == null) {
                instance = new ThreadSingleton();
            }

            return instance;
        }
    }

    public sealed class TimedSingleton {
        static object lockObj = new object(); 
        static TimedSingleton instance;
        static DateTime lastUpdate;

        TimedSingleton() {
            lastUpdate = DateTime.Now;
        }

        static bool Expired() {
            var expirationTime = lastUpdate.AddSeconds(5);
            return DateTime.Now >= expirationTime;
        }

        public static TimedSingleton Instance() {
            if (instance == null || Expired()) {
                lock (lockObj) {
                    if (instance == null || Expired()) {
                        instance = new TimedSingleton();
                    }
                }
            }

            return instance;
        }
    }
}
