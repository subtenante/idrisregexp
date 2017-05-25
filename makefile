build: clean
	idris --build regexp.ipkg

clean:
	-cd src/ && rm -rf *.ibc

repl:
	cd src/ && idris -p effects -p lightyear Main

