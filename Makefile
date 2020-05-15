all: CastCalculus

parser.hs : parser.y
	happy parser.y
    
CastCalculus :  parser.hs CastCalculus.hs
	ghc --make CastCalculus
    
clean:
	rm -f CastCalculus parser.hs *.o *.hi
    
