using System;
using System.Collections;
using System.Collections.Generic;

namespace Zadanie4 {

    class ComparisonToIComparerAdapter<T> : IComparer {
        Comparison<T> cmp;

        public ComparisonToIComparerAdapter(Comparison<T> cmp) {
            this.cmp = cmp;
        }

        public int Compare(Object x, Object y) {
            return cmp((T)x, (T)y);
        }
    }


    class Example {
        public static void Main() {
            var xs = new ArrayList() {1, 5, 3, 3, 2, 4, 3}; 
            Comparison<int> cmp = (x, y) => x.CompareTo(y);

            foreach (var x in xs) {
                Console.Write("{0} ", x);
            }
            Console.WriteLine();

            xs.Sort(new ComparisonToIComparerAdapter<int>(cmp));
            foreach (var x in xs) {
                Console.Write("{0} ", x);
            }
            Console.WriteLine();
        }
    }
}