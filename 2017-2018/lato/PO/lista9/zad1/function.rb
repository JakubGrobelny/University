# Jakub Grobelny
# Pracownia PO, czwartek, s. 108
# L9z1, Funkcja.
# Implementacja klasy „Function”, która reprezentuje
# funkcje jednoargumentowe.
# function.rb
# 2018-05-15

require './plot.rb'

class Function

    # Konstruktor.
    def initialize(proc)
        throw :invalid_function unless proc.is_a?(Proc)
        @proc = proc
    end

    # Wartoœæ funkcji w punkcie x.
    def value(x)
        @proc.call(x)
    end

    # Pomocnicza funkcja porównuj¹ca dwie liczby zmiennoprzecinkowe.
    private def equals?(a, b, epsilon)
        if (a <= b + epsilon) and (a >= b - epsilon)
            return true
        else 
            return false
        end
    end

    # Funkcja rekurencyjnie znajduj¹ca miejsce zerowe w zadanym przedziale.
    def zero(a, b, e)

        # Miejsce zerowe na krañcu przedzia³u.
        if value(a) == 0
            return a
        elsif value(b) == 0
            return b
        # Je¿eli krañce przedzia³u s¹ równe to nie ma miejsca zerowego.
        elsif equals?(a, b, e)
            return nil
        end

        s = (a + b).to_f / 2
        # Œrodek przedzia³u.

        # Je¿eli s jest miejscem zerowym to zwracamy wynik.
        if equals?(value(s), 0, e)
            return s
        # W przeciwnym razie po³owimy przedzia³.
        else
            if value(s) * value(a) < 0
                return zero(a, s, e)
            else return zero(s, b, e)
            end
        end
    end

    # Funkcja rekurencyjnie obliczaj¹ca pole pod wykresem funkcji.
    def area(a, b)

        # Sta³a oznaczaj¹ca dok³adnoœæ z jak¹ wyliczane jest pole.
        @@accuracy = 0.001

        # Je¿eli przedzia³ jest pusty to pole wynosi zero.
        if a >= b
            return 0
        # W przeciwnym razie dodajemy pole trapezu do reszty pola.
        else
            y1 = value(a).to_f
            y2 = value(a + @@accuracy).to_f
            return ((y1 + y2) * 0.5 * @@accuracy) + area(a + @@accuracy, b)
        end
    end

    # Funkcja obliczaj¹ca wartoœæ pochodnej funkcji z definicji.
    def derivative(x)
        @@h = 0.00001
        return (value(x + @@h) - value(x)) / @@h
    end
end

f = Function.new(Proc.new {|x| x**3})
puts "f(3): ", f.value(3)
puts "zero: ", f.zero(-2, 4, 0.001)
puts "area: ", f.area(0, 2)
puts "f' ", f.derivative(2)
f.draw(-3, 3, 0.3)
puts

g = Function.new(Proc.new {|x| 2 * x})
puts g.value(0)
puts g.zero(-10, 10, 0.1)
puts g.area(0, 1)
puts g.derivative(3)
g.draw(-10, 10, 0.2)
puts

h = Function.new(Proc.new {|x| Math.sin(x)})
puts h.value(Math::PI / 2.0)
puts h.zero(-0.1, 0.1, 0.1)
puts h.area(0, Math::PI)
puts h.derivative(0)
h.draw(-12, 12, 0.1)
puts

d = Function.new(Proc.new {|x| 3})
d.draw(-10, 10, 0.01)