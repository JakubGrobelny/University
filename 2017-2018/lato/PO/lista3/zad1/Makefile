# Kompilowanie programu testuj¹cego
make:
	mcs -out:lists-test example.cs -r:list.dll

# Kompilowanie modu³u list.dll
dll:
	mcs -target:library -out:list.dll list.cs

# Usuwanie programu testuj¹cego
clean:
	$(RM) lists-test

# Usuwanie skompilowanego modu³u list.dll
clean-dll:
	$(RM) list.dll