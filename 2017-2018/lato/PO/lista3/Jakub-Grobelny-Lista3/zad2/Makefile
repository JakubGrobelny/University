# Kompilowanie programu testuj�cego
make:
	mcs -out:dict-test example.cs -r:dictionary.dll

# Kompilowanie modu�u dictionary.dll
dll:
	mcs -target:library -out:dictionary.dll dictionary.cs

# Usuwanie programu testuj�cego
clean:
	$(RM) dict-test

# Usuwanie skompilowanego modu�u dictionary.dll
clean-dll:
	$(RM) dictionary.dll