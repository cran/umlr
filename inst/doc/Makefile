makeall:
	make makedev
	make clean

makedev:
	make umlr.pdf

umlr.pdf: umlr.tex maia.sty
	pdflatex umlr.tex

umlr.tex: umlr.rnw
	R CMD Sweave umlr.rnw

clean:
	rm *.tex
	rm *.aux
	rm *.log

