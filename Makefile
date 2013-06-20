.PHONY: clean

clean:
	rm -rf tmp
	rm *.aux *.eventlog *.ps *.hp *.pdf
	cabal clean