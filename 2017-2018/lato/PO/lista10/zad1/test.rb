# Jakub Grobelny
# Pracownia PO, czwartek, s. 108
# L10z1, Sortowanie kolekcji
# Testy sortowania
# test.rb
# 2018-05-15

require './sort.rb'
require './collection.rb'

# Wymagana jest nazwa pliku jako argument.
throw :invalid_arguments unless ARGV.size == 1
file_name = ARGV[0]
elements = File.readlines(file_name)

stack = Stack.new()

# Wype³nianie kolekcji.
for element in elements
    stack.push(element.strip)
end

# Sortowanie z mierzeniem czasu.
start = Time.now
a = Sort::sort1(stack)
finish = Time.now
t1 = finish - start

start = Time.now
b = Sort::sort2(stack)
finish = Time.now
t2 = finish - start

puts a.to_s
puts b.to_s
puts stack.to_s
puts "Merge sort time: ",  t1
puts "Bubble sort time: ", t2
puts
