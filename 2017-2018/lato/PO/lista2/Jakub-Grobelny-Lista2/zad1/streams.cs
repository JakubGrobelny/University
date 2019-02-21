// Jakub Grobelny 300481
// 7 marca 2018
// Programowanie Obiektowe - Lista 2, zadanie 1

// Implementacja strumieni liczb naturalnych, liczb pierwszych, losowych liczb
// oraz strumieni losowych łańcuchów znaków.

// Każda z poniższych klas posiada następujące metody:
// Next() - zwraca aktualny element ze strumienia po czym generuje następny.
// Eos() - koniec strumienia (prawda, jeżeli nie można wygenerować kolejnych elementów).
// Reset() - przywrócenie strumienia do początkowego stanu.

using System;
using System.Text;

namespace Streams
{
    class IntStream
    {
        protected Int32 currentVal;

        virtual public Int32 Next()
        {
            return currentVal++;
        }

        virtual public bool Eos() 
        {
            return (Int32.MaxValue == currentVal);
        }

        virtual public void Reset()
        {
            currentVal = 0;
        }

        public IntStream()
        {
            Reset();
        }
    }

    class PrimeStream : IntStream
    {
        private bool Prime(int number)
        {
            Int32 number_sqrt = (Int32)Math.Sqrt(number);

            for (Int32 x = 2; x <= number_sqrt; x++)
                if (number % x == 0)
                    return false;

            return true;
        }

        override public Int32 Next()
        {            
            while (!Eos())
                if (Prime(++currentVal))
                    break;

            return currentVal;
        }

        override public void Reset()
        {
            currentVal = 1;
        }

        public PrimeStream()
        {
            Reset();
        }
    }

    class RandomStream : IntStream
    {
        private static Random randomizer = new Random();

        override public void Reset()
        {
            currentVal = randomizer.Next();
        }

        override public Int32 Next()
        {
            Reset();

            return currentVal;
        }

        override public bool Eos()
        {
            return false;
        }

        public RandomStream()
        {
            Reset();
        }
    }

    class RandomWordStream
    {
        StringBuilder stringBuilder;

        private PrimeStream primeStream;
        private RandomStream randomStream;

        public string Next()
        {
            Int32 length = primeStream.Next();

            stringBuilder.Clear();

            Int32 start = (Int32)'!';
            Int32 range = (Int32)'~' - start;

            for (int i = 0; i < length; i++)
            {
                char c = (char)(randomStream.Next() % range + start);
                stringBuilder.Append(c);
            }

            return stringBuilder.ToString();
        }

        public bool Eos()
        {
            return primeStream.Eos();
        }

        public void Reset()
        {
            primeStream.Reset();
            randomStream.Reset();
            stringBuilder.Clear();
        }

        public RandomWordStream()
        {
            primeStream = new PrimeStream();
            randomStream = new RandomStream();
            stringBuilder = new StringBuilder();
        }
    }
}