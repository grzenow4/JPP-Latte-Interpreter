main:
	ghc --make -outputdir build -o interpreter Main.hs

clean:
	rm -rf build interpreter
