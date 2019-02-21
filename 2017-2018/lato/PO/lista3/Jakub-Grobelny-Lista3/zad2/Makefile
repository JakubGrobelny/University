# Kompilowanie programu testuj¹cego
make:
	mcs -out:dict-test example.cs -r:dictionary.dll

# Kompilowanie modu³u dictionary.dll
dll:
	mcs -target:library -out:dictionary.dll dictionary.cs

# Usuwanie programu testuj¹cego
clean:
	$(RM) dict-test

# Usuwanie skompilowanego modu³u dictionary.dll
clean-dll:
	$(RM) dictionary.dll