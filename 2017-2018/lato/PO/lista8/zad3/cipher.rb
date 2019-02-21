# Jakub Grobelny
# Pracownia PO, czwartek, s. 108
# L8z3, Szyfrowanie.
# Implementacja klas reprezentuj�cych zaszyfrowany i niezaszyfrowany napis.
# cipher.rb
# 2018-04-19

# Niezaszyfrowany napis.
class Unencrypted
    
    # Konstruktor.
    def initialize(text)
        @text = text
    end

    # Zamiana na napis.
    def to_s()
        return @text
    end
    
    # Szyfrowanie.
    def encrypt(key)
        encrypted = ""

        for i in 0...@text.length
            encrypted += key.member?(@text[i]) ? key[@text[i]] : @text[i]
        end

        return Encrypted.new(encrypted)
    end
end

# Zaszyfrowany napis.
class Encrypted

    # Konstruktor
    def initialize(text)
        @text = text
    end

    # Zamiana na napis.
    def to_s()
        return @text
    end

    # Odszyfrowywanie.
    def decrypt(key)
        decrypted = ""
        invkey = key.invert()

        for i in 0...@text.length
            decrypted += invkey.member?(@text[i]) ? invkey[@text[i]] : @text[i]
        end

        return Unencrypted.new(decrypted)
    end

end