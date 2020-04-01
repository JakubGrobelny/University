using System;

class Example {
    public static void Main() {
        TestBefore();
        TestAfter();
    }

    public static void TestBefore() {
        var items = new Before.Item [] { 
            new Before.Item() {
                Price = 21.37m,
                Name  = "foo"
            },

            new Before.Item() {
                Price = 4.2m,
                Name  = "bar"
            }
         };

        var register = new Before.CashRegister();
        Console.Write(register.PrintBill(items));
    }

    public static void TestAfter() {
        var items = new After.CategorizedItem [] {
            new After.CategorizedItem() {
                Price = 21.37m,
                Name  = "foo",
                Category = "FOOO"
            },

            new After.CategorizedItem() {
                Price = 73.21m,
                Name  = "bar",
                Category = "zaaaaa"
            }
        };

        var polishRegister = new After.CashRegister(
            new After.PolishVATCalculator()
        );

        var swissRegister  = new After.CashRegister(
            new After.SwissVATCalculator()
        );

        var plBill = polishRegister.PrintBill(
            items, 
            new After.CategorySorter()
        );

        var swBill = swissRegister.PrintBill(
            items, 
            new After.AlphabeticSorter()
        );

        Console.Write("Posortowane według kategorii:\n{0}", plBill);
        Console.WriteLine("Cena: {0}\n", polishRegister.CalculatePrice(items));

        Console.Write("Posortowane według nazw:\n{0}", swBill);
        Console.WriteLine("Cena: {0}\n", swissRegister.CalculatePrice(items));
    }
}