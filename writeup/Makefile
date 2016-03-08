
tables = ./tables/predictions.tex ./tables/error_fe.tex ./tables/continuous_vars.tex

plots = ./plots/box_plots_by_occupation.pdf ./plots/knowledge_by_occupation.pdf ./plots/social_by_occuation.pdf ./plots/predicted_v_actual.pdf

$(tables) $(plots): ../analysis/kwowR/inst/extdata/mturk_cooked.rds ../analysis/kwow.R
	cd ../analysis && ./kwow.R	

kwow.pdf: kwow.bib kwow.tex $(tables) $(plots)
	pdflatex -interaction=nonstopmode kwow
	bibtex kwow
	pdflatex -interaction=nonstopmode kwow
	pdflatex -interaction=nonstopmode kwow

clean:
	rm *.aux *.log *.out *.bbl *.blg *.dvi *.aux *.fff *.lof *.out 
