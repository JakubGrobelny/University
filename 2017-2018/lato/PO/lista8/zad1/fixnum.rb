# Jakub Grobelny
# Pracownia PO, czwartek, s. 108
# L8z1, Rozszerzenie klasy Fixnum
# Rozszerzenie klasy Fixnum o metody
# factors, ack, perfect?, to_words
# fixnum.rb
# 2018-04-19

class Fixnum

    # S³ownik przyporz¹dkuj¹cy cyfrom ich s³own¹ reprezentacjê.
    @@words = {1 => "one", 2 => "two", 3 => "three", 4 => "four",
               5 => "five", 6 => "six", 7 => "seven", 8 => "eight",
               9 => "nine", 0 => "zero"}

    # Metoda zwracaj¹ca tablicê dzielników liczby.
    def factors()
        sq_root = Math.sqrt(self)
        
        divisors = []

        for i in 1..sq_root+1
            if self % i == 0
                divisors.push(i)
            end
        end

        divisors.push(self)

        return divisors
    end

    # Metoda obliczaj¹ca funkcjê Ackermanna.
    # ack(n, m) <=> n.ack(m)
    def ack(y)
        
        if self == 0
            return y + 1
        elsif y == 0
            return (self - 1).ack(1)
        else
            return (self-1).ack((self.ack(y - 1)))
        end
    end

    # Metoda sprawdzaj¹ca, czy liczba jest doskona³a.
    def perfect?()

        divisors = self.factors()
        sum = 0

        for i in 0...divisors.length - 1
            sum += divisors[i]
        end

        if (sum == self)
            return true
        else
            return false
        end
    end

    # Metoda zamieniaj¹ca liczbê na jej s³own¹ reprezentacjê.
    def to_words()

        num = self
        words = []

        while num != 0
            words.push(@@words[num % 10])
            num /= 10
        end

        result = ""
        words = words.reverse
        for i in 0...words.length
            result += words[i] + " "
        end

        return result
    end
    
end