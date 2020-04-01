using System;
using System.Collections.Generic;
using System.Linq;

namespace After {
    public interface ITaxCalculator {
        Decimal CalculateTax(Decimal price);
    }

    public interface IItem {
        Decimal Price {get; set;}
        string Name   {get; set;}
    }

    public interface IItemSorter<I> where I : IItem {
        IEnumerable<I> SortItems(IEnumerable<I> items);
    }

    public class CashRegister {
        private ITaxCalculator taxCalc;

        public CashRegister(ITaxCalculator calc) {
            this.taxCalc = calc;
        }

        public Decimal CalculatePrice(IEnumerable<IItem> items) {
            return items.Sum(
                item => taxCalc.CalculateTax(item.Price) + item.Price
            );
        }

        public string PrintBill<I>(IEnumerable<I> items, IItemSorter<I> sorter) 
        where I : IItem {
            string bill = "";
            foreach (var item in sorter.SortItems(items)) {
                bill += String.Format(
                    "towar {0} : cena {1} + podatek {2}\n",
                    item.Name, item.Price, taxCalc.CalculateTax(item.Price)
                );
            }
            return bill;
        }
    }

    // Przykładowe klasy używające powyższych interfejsów

    public class PolishVATCalculator : ITaxCalculator {
        public Decimal CalculateTax(Decimal price) {
            return price * 0.23m;
        }
    }

    public class SwissVATCalculator : ITaxCalculator {
        public Decimal CalculateTax(Decimal price) {
            return price * 0.08m;
        }
    }

    public class CategorizedItem : IItem {
        public Decimal Price {get; set;}
        public string Name   {get; set;}
        public string Category {get; set;}
    }

    public class CategorySorter : IItemSorter<CategorizedItem> {
        public IEnumerable<CategorizedItem> SortItems(
            IEnumerable<CategorizedItem> items
        ) {
            return items.OrderBy(item => item.Category);
        }
    }

    public class AlphabeticSorter : IItemSorter<IItem> {
        public IEnumerable<IItem> SortItems(IEnumerable<IItem> items) {
            return items.OrderBy(item => item.Name);
        }
    }
}