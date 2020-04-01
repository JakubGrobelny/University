// Kod po zmianach:

using System;

namespace Before {
    public class TaxCalculator {
        public Decimal CalculateTax(Decimal price) { 
            return price * 0.22m;
        }
    }

    public class Item {
        public Decimal Price { get; set; }
        public string Name { get; set; }
    }

    public class CashRegister {
        public TaxCalculator taxCalc = new TaxCalculator();

        public Decimal CalculatePrice(Item[] items) {
            Decimal price = 0;
            foreach (var item in items) {
                price += item.Price + taxCalc.CalculateTax(item.Price);
            }
            return price;
        }

        public string PrintBill(Item[] items) {
            string result = "";
            foreach (var item in items) {
                var tax = taxCalc.CalculateTax(item.Price);
                result += String.Format(
                    "towar {0} : cena {1} + podatek {2}\n", 
                    item.Name, item.Price, tax
                );
            }
            return result;
        }
    }
}