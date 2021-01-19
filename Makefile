#################################################################
#     COMP90045 Programming Language Implementation, Stage 3
# 
#                          Wallaby
# 
#                     Karl Flores 760493
#                    Marco Marasco 834482
#                   Austen McClernon 834063
#################################################################

#################################################################
# Makefile for Roo parser program.
#################################################################

Roo: RooAST.hs RooParser.hs PrettyRoo.hs Roo.hs Compiler.hs SymbolTable.hs
	ghc Roo.hs

clean:
	rm *.o *.hi
	rm Roo

