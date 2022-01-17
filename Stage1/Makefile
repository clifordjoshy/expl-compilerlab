all: Compiler

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y
    
Compiler : Tokens.hs Grammar.hs Compiler.hs Tree.hs
	ghc --make Compiler -outputdir=__intermediate__
    
clean:
	rm -rf Compiler Grammar.hs Tokens.hs __intermediate__ out.xsm

