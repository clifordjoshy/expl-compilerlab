all: Compiler

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

LabelLink.hs : LabelLink.x
	alex LabelLink.x
    
Compiler : Tokens.hs Grammar.hs Compiler.hs SyntaxTree.hs CodeGen.hs LabelLink.hs
	ghc --make Compiler -outputdir=__intermediate__
    
clean:
	rm -rf Compiler Grammar.hs Tokens.hs Linker.hs __intermediate__

