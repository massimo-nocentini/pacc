ps: 
	# compile two times in order to have all correct references setted
	latex Elaborato.tex && dvips -t a4 -Ppdf Elaborato.dvi
	#latex Elaborato.tex && dvips -t a4 -Ppdf Elaborato.dvi

pdf: ps
	ps2pdf Elaborato.ps Elaborato.pdf
