pdf: 
	lualatex Elaborato.tex
ps: 
	latex Elaborato.tex && dvips -t a4 -Ppdf Elaborato.dvi

clean:
	rm *.toc *.out *.log *.aux
