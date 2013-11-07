kwow.pdf: kwow.bib kwow.tex ./plots/hist.pdf 
	pdflatex -interaction=nonstopmode kwow
	bibtex kwow
	pdflatex -interaction=nonstopmode kwow
	pdflatex -interaction=nonstopmode kwow

./plots/hist.pdf: ../code/R/kwow.R
	cd ../code/R; ./kwow.R

clean:
	rm *.aux *.log *.out *.bbl *.blg *.dvi *.aux *.fff *.lof *.out 