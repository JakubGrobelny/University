using System;

namespace Before {
    public class ReportPrinter {
        public string GetData() {
            Console.WriteLine("Getting data...");
            return "some data";
        }

        public void FormatDocument() {
            Console.WriteLine("Formatting document...");
        }

        public void PrintReport() {
            GetData();
            FormatDocument();
            Console.WriteLine("Printing report...");
        }
    }

}


