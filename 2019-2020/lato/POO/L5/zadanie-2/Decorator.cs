using System;
using System.IO;
using System.Text;

namespace Zadanie2 {

    class CaesarStream : Stream {
        Stream stream;
        int offset;

        public CaesarStream(Stream stream, int offset) {
            this.stream = stream;
            this.offset = offset;
        }

        public override void Write(byte[] buffer, int offset, int count) {
            var newBuffer = new byte[buffer.Length];
            for (int i = 0; i < buffer.Length; i++) {
                newBuffer[i] = (byte)(((int)buffer[i] + this.offset) % 255);
            }
            this.stream.Write(newBuffer, offset, count);
        }

        public override int Read(byte[] buffer, int offset, int count) {
            var result = this.stream.Read(buffer, offset, count);
            for (int i = 0; i < buffer.Length; i++) {
                buffer[i] = (byte)(((int)buffer[i] + this.offset) % 255);
            }
            return result;
        }

        public override long Length {
            get { return this.stream.Length;  }
        }

        public override bool CanRead {
            get { return this.stream.CanRead; }
        }

        public override bool CanSeek {
            get { return this.stream.CanSeek; }
        }

        public override bool CanWrite {
            get { return this.stream.CanWrite; }
        }

        public override long Position {
            get { return this.stream.Position;  }
            set { this.stream.Position = value; }
        }

        public override void Flush() {
            this.stream.Flush();
        }

        public override long Seek(long offset, SeekOrigin origin) {
            return this.stream.Seek(offset, origin);
        }

        public override void SetLength(long length) {
            this.stream.SetLength(length);
        }
    }

    class Example {
        public static void Main() {
            const string input = "jakis przykladowy napis";

            var newFile = File.Create("test.txt");
            CaesarStream writer = new CaesarStream(newFile, 5);
            writer.Write(Encoding.ASCII.GetBytes(input));
            newFile.Close();

            var file1 = File.OpenRead("test.txt");
            CaesarStream reader = new CaesarStream(file1, -5);
            var bytes = new byte[1000];
            reader.Read(bytes);

            Console.WriteLine("przed: '{0}'", input);
            Console.WriteLine("potem: '{0}'", Encoding.ASCII.GetString(bytes));

            var file2 = File.OpenRead("test.txt");
            var bytes2 = new byte[1000];
            file2.Read(bytes2);
            Console.WriteLine(
                "zaszyfrowane: {0}", Encoding.ASCII.GetString(bytes2)
            );
        }
    }
}

