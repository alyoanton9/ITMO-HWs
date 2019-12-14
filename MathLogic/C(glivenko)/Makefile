all: compile

run:
	./Main
compile: 
	ghc Main.hs
gen:
	happy Parser.y
	alex Lexer.x
pack:	
	zip ../pack.zip -r *
clean:
	rm -rf *.hi *.o Main Parser.hs Lexer.hs
.PHONY: run all compile gen pack clean