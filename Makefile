test: clean build
	./Main

build:
	ghc -O2 -fforce-recomp --make Main.hs

clean:
	-rm *.o ./Main *.hi
