all: Compiler

Tokens.hs : Tokens.x
	alex Tokens.x

Parser.hs : Parser.y
	happy Parser.y

LabelLink.hs : LabelLink.x
	alex LabelLink.x
    
Compiler : Tokens.hs Parser.hs ParserState.hs Compiler.hs SyntaxTree.hs SymbolTable.hs LabelLink.hs XsmGen.hs TypeTable.hs CodeGen.hs
	ghc --make Compiler -outputdir=__intermediate__
    
clean:
	rm -rf Compiler Parser.hs Tokens.hs LabelLink.hs __intermediate__

