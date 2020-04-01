using System;
public class Test {
    public static void Main() {
        var oldPrinter = new Before.ReportPrinter();
        var newPrinter = new After.ReportPrinter();

        Console.WriteLine("== Test kodu przed zmianami ==");
        oldPrinter.PrintReport();

        Console.WriteLine();

        Console.WriteLine("== Test kodu po zmianach ==");
        newPrinter.PrintReport();
    }
}