## Sisserou -- A linguistic toy based on System F Omega   ##
##                                                        ##
## Copyright 2K14 DP Constructions                        ##
##            and K.D.P.Ross <KDPRoss@gmail.com>          ##
##                                                        ##
## This codebase is licensed for the following purposes   ##
## only:                                                  ##
##                                                        ##
## - study of the code                                    ##
##                                                        ##
## - use of the unaltered code to compile the interpreter ##
##   for noncommercial educational and entertainment      ##
##   purposes only                                        ##
##                                                        ##
## - gratis redistribution of the code in entirety and in ##
##   unaltered form for any aforementioned purpose        ##
##                                                        ##
## The code may not be used for any other purposes,       ##
## including but not limited to:                          ##
##                                                        ##
## - any commercial purpose                               ##
##                                                        ##
## - use by any governmentally-affiliated organisation    ##
##                                                        ##
## - connection to any external system for any useful     ##
##   purpose whatsoever                                   ##





HASKELL=ghc
EXEC=Sisserou

$(EXEC): Sisserou.hs Parser.hs Eval.hs  Main.hs
	$(HASKELL) Main.hs -o $(EXEC)

clean:
	rm -f *.hi *.o *# *~ $(EXEC)
