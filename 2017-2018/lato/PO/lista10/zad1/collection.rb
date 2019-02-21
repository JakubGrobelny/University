# Jakub Grobelny
# Pracownia PO, czwartek, s. 108
# L10z1, Sortowanie kolekcji
# Implementacja kolekcji (stosu)
# collection.rb
# 2018-05-15

class Stack
    # Konstruktor
    def initialize()
        @arr = []
    end

    # Metoda dodaj¹ca element na wierzch stosu.
    def push(element)
        @arr.push(element)
    end

    # Metoda zamieniaj¹ca kolekcjê na napis.
    def to_s()
        return @arr.to_s
    end

    # Metoda usuwaj¹ca i zwracaj¹ca element z wierzchu stosu.
    def pop()
        throw :stack_empty unless length > 0
        return @arr.pop
    end

    # Metoda zwracaj¹ca element znajduj¹cy siê na wierzchu stosu.
    def peek()
        throw :stack_empty unless length > 0
        return @arr[-1]
    end

    # Metoda zwracaj¹ca liczbê elementów na stosie.
    def length()
        return @arr.size
    end

    # Metoda zwracaj¹ca i-ty element ze stosu.
    def get(i)
        throw :invalid_index unless i < length and i >= 0
        return @arr[i]
    end

    # Metoda ustawiaj¹ca i-ty element stosu na wartoœæ „val”.
    def set(i, val)
        throw :invalid_index unless i < length and i >= 0
        @arr[i] = val
    end

    # Metoda tworz¹ca kopiê stosu.s
    def clone()
        copy = Stack.new()
        for element in @arr
            copy.push(element)
        end
        return copy
    end

    # Metoda zamieniaj¹ca i-ty i j-ty element.
    def swap(i, j)
        throw :invalid_index unless i < length and j < length and i >= 0 and j >= 0
        temp = @arr[i]
        @arr[i] = @arr[j]
        @arr[j] = temp
    end
end