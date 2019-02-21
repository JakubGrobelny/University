# Jakub Grobelny
# Pracownia PO, czwartek, s. 108
# L10z1, Sortowanie kolekcji
# Klasa sortuj¹ca kolekcje na dwa sposoby.
# sort.rb
# 2018-05-15


class Sort

    # merge sort
    def self.sort1(collection)
        throw :invalid_collection unless valid_collection?(collection)
        c = collection.clone
        merge_sort(c, 0, c.length - 1)
        return c
    end

    # bubble sort
    def self.sort2(collection)
        throw :invalid_collection unless valid_collection?(collection)
        c = collection.clone

        for i in 0...c.length
            for j in 1...c.length - i
                if (c.get(j) < c.get(j - 1))
                    c.swap(j, j - 1)
                end
            end
        end

        return c
    end
    
    # Pomocnicze metody prywatne.
    private

    def self.valid_collection?(collection)
        if collection.respond_to?("length") and \
           collection.respond_to?("swap")   and \
           collection.respond_to?("get")    and \
           collection.respond_to?("set")    and \
           collection.respond_to?("clone")
            return true
        end
        return false
    end

    # Funkcja sortuj¹ca za pomoc¹ sortowania przez scalanie.
    def self.merge_sort(collection, s, e)
        if (s != e)
            middle = (s + e) / 2;
            merge_sort(collection, s, middle)
            merge_sort(collection, middle + 1, e)
            merge(collection, s, middle + 1, middle, e)
        end
    end

    # Funkcja scalaj¹ca dwie podkolekcje.
    def self.merge(collection, s1, s2, e1, e2)
        temp = Array.new(e2 - s1 + 1)
        ptr1 = s1
        ptr2 = s2
        i = 0

        # Scalanie
        while (ptr1 <= e1 and ptr2 <= e2)
            if collection.get(ptr1) >= collection.get(ptr2)
                temp[i] = collection.get(ptr2)
                ptr2 += 1
            else
                temp[i] = collection.get(ptr1)
                ptr1 += 1
            end
            i += 1
        end

        ptr = (ptr1 <= e1) ? ptr1 : ptr2
        e   = (ptr1 <= e1) ? e1 : e2

        # Przepisywanie pozosta³ych elementów.
        while (ptr <= e)
            temp[i] = collection.get(ptr)
            ptr += 1
            i += 1
        end

        # Przepisywanie wyniku do oryginalnej tablicy
        ptr = s1
        for element in temp
            collection.set(ptr, element)
            ptr += 1
        end
    end
end