# Kompilowanie programu testuj�cego
make:
	mcs -out:lists-test example.cs -r:list.dll

# Kompilowanie modu�u list.dll
dll:
	mcs -target:library -out:list.dll list.cs

# Usuwanie programu testuj�cego
clean:
	$(RM) lists-test

# Usuwanie skompilowanego modu�u list.dll
clean-dll:
	$(RM) list.dll