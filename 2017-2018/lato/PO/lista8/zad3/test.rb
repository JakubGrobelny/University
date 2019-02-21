# Jakub Grobelny
# Pracownia PO, czwartek, s. 108
# L8z3, Szyfrowanie.
# Test implementacji szyfrowania.
# test.rb
# 2018-04-19

load 'cipher.rb'

key = {}
n = 0

# Wczytywanie liczby szyfrowanych znaków.
puts 'Enter the number of characters in the key: '
n = gets.to_i

# Wczytywanie znaków do klucza.
for i in 0...n
    puts "Key: "
    k = gets
    puts "Value: "
    v = gets
    key[k.chr] = v.chr
    puts
end

puts "Enter the word to encrypt: "

word = gets

before = Unencrypted.new(word)
after = Encrypted.new(before.encrypt(key).to_s)

# Wypisywanie wyników.
puts "Encrypted: " + after.to_s
puts "Unencrypted: " + after.decrypt(key).to_s
