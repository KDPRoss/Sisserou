run: compile
	clear
	rlwrap stack exec sisserou-exe

compile:
	stack build

clean:
	stack clean
	rm sisserou.cabal

