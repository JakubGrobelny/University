using System;
using System.IO;

namespace Zadanie1 {

    public interface ILogger {
        void Log(string msg);
    }

    public enum LogType { 
        None, 
        Console, 
        File 
    }

    public class NullLogger : ILogger {
        public void Log(string msg) {}
    }

    public class ConsoleLogger : ILogger {
        public void Log(string msg) {
            Console.WriteLine(msg);
        }
    }

    public class FileLogger : ILogger {
        private StreamWriter file;

        public void Log(string msg) {
            file.WriteLine(msg);
        }

        public FileLogger(string path) {
            if (path == null) {
                throw new ArgumentException("FileLogger: null path");
            }

            this.file = new StreamWriter(path);
        }
    }

    public class LoggerFactory {
        private static LoggerFactory instance;

        private LoggerFactory() {}

        public ILogger GetLogger(LogType type, string parameter = null) { 
            switch (type) {
                case LogType.None:
                    return new NullLogger();
                case LogType.Console:
                    return new ConsoleLogger();
                case LogType.File:
                    return new FileLogger(parameter);
                default:
                    throw new ArgumentException();
            }
        }

        public static LoggerFactory Instance() { 
            if (instance == null) {
                instance = new LoggerFactory();                
            }

            return instance;
        }
    }

    public class Example {
        public static void Main() {
            var factory = LoggerFactory.Instance();
            ILogger logger1 = factory.GetLogger(LogType.File, "./log");
            ILogger logger2 = factory.GetLogger(LogType.Console);
            ILogger logger3 = factory.GetLogger(LogType.None);

            logger1.Log("foo bar");
            logger2.Log("foo bar");
            logger3.Log("foo bar");
        }
    }
}