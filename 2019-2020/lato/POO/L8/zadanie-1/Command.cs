using System;
using System.IO;
using System.Net;
using System.Collections.Concurrent;
using System.Threading;
using System.Threading.Tasks;

namespace Zadanie1 {

    public interface IFileCommand {
        void Execute();
    }

    public class FtpDownloadCommand : IFileCommand {
        string address;
        string name;

        public FtpDownloadCommand(string address, string name) {
            this.address = address;
            this.name    = name;
        }

        public void Execute() {
            using (var client = new WebClient()) {
                client.DownloadFile(this.address, this.name);
            }
        }
    }

    public class HttpDownloadCommand : IFileCommand {
        string address;
        string name;

        public HttpDownloadCommand(string address, string name) {
            this.address = address;
            this.name    = name;
        }

        public void Execute() {
            using (var client = new WebClient()) {
                client.DownloadFile(this.address, this.name);
            }
        }
    }

    public class FillFileCommand : IFileCommand {
        string fileName;
        int    size;

        public FillFileCommand(string fileName, int size) {
            this.fileName = fileName;
            this.size     = size;
        }

        public void Execute() {
            Random random = new Random();
            Byte[] bytes  = new Byte[this.size];

            random.NextBytes(bytes);
            File.WriteAllBytes(fileName, bytes);
        }
    }

    public class CopyFileCommand : IFileCommand {
        string from;
        string to;

        public CopyFileCommand(string from, string to) {
            this.from = from;
            this.to   = to;
        }

        public void Execute() {
            File.Copy(from, to);
        }
    }

    public class CommandQueue {
        private ConcurrentQueue<IFileCommand> queue;
        private string dir;
        private int fileCounter = 1;

        private IFileCommand randomCommand() {
            var random = new Random();
            int commandNumber = random.Next(0, 4);
            string output = this.dir + "/file" + fileCounter++;

            switch (commandNumber) {
                case 1: {// kopia
                    string from = dir + "/file" + random.Next(0, fileCounter);
                    return new CopyFileCommand(from, output);
                }
                case 2: // nowy plik
                    return new FillFileCommand(output, 1024);
                case 3: // ftp
                    // nie mam przykładu dla ftp więc zamiast tego tworzy http
                default: // http
                    return new HttpDownloadCommand(
                        "http://www.ii.uni.wroc.pl/~wzychla/ra2J2K/oop.html", 
                        output
                    );
            }
        }

        private void Consume() {
            while (true) {
                IFileCommand command = null;
                if (this.queue.TryDequeue(out command)) {
                    command.Execute();
                    Thread.Sleep(500);
                }
            }
        }

        public CommandQueue(string dir, Nullable<int> commandCount = null) {
            this.queue = new ConcurrentQueue<IFileCommand>();
            this.dir = dir;

            if (!Directory.Exists(dir)) {
                Directory.CreateDirectory(dir);
                new FillFileCommand(dir + "/file0", 1024).Execute();
            }

            Thread consumer1 = new Thread(Consume);
            Thread consumer2 = new Thread(Consume);

            consumer1.Start();
            consumer2.Start();

            while (commandCount == null || commandCount > 0) {
                var command = randomCommand();
                this.queue.Enqueue(command);
                Thread.Sleep(500);

                if (commandCount != null) {
                    commandCount--;
                }
            }
        }

    }

    public class Example {
        public static void Main() {
            new CommandQueue("test-1", 20);
        }
    }
}


