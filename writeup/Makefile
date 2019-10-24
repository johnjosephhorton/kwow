project_name = kwow
ts := $(shell /bin/date "+%Y-%m-%d---%H-%M-%S")

package_list.txt:
	 find ../analysis/ -name *R -print0 | xargs -0 grep -h 'library*' | sed 's/library//g' | sed 's/#//g' | sed 's/ //g' | sort | uniq| sed 's/(//g' | sed 's/)//g' > package_list.txt

install_packages: package_list.txt
	cat package_list.txt | xargs -I {} Rscript -e 'install.packages("{}")'


tables = 

tables += tables/predictions.tex
tables/predictions.tex: ../analysis/table_predictions.R
	cd ../analysis && ./table_predictions.R 

tables += tables/error.tex
tables/error.tex: ../analysis/table_error.R
	cd ../analysis && ./table_error.R

#tables += tables/continuous_vars.tex

#plots = ./plots/box_plots_by_occupation.pdf ./plots/knowledge_by_occupation.pdf ./plots/social_by_occuation.pdf 

plots =

plots += plots/predicted_v_actual.pdf
plots/predicted_v_actual.pdf: ../analysis/plot_predicted_v_actual.R
	cd ../analysis && ./plot_predicted_v_actual.R

plots += plots/knowledge_by_occupation.pdf
plots/knowledge_by_occupation.pdf: ../analysis/plot_knowledge_by_occupation.R
	cd ../analysis && ./plot_knowledge_by_occupation.R

plots += plots/social_by_occupation.pdf
plots/social_by_occupation.pdf: ../analysis/plot_social_by_occupation.R
	cd ../analysis && ./plot_social_by_occupation.R

plots += plots/box_plots_by_occupation.pdf
plots/box_plots_by_occupation.pdf: ../analysis/plot_box_plots_by_occupation.R
	cd ../analysis && ./plot_box_plots_by_occupation.R


# $(tables) $(plots): ../analysis/kwowR/inst/extdata/mturk_cooked.rds ../analysis/kwow.R
# 	cd ../analysis && ./kwow.R	

$(project_name).pdf: $(project_name).bib $(project_name).tex $(tables) $(plots) $(parameter_files)
	pdflatex -interaction=batchmode $(project_name)
	bibtex $(project_name)
	pdflatex -interaction=batchmode $(project_name)
	pdflatex -interaction=batchmode $(project_name)

purge:
	cd plots && mv *pdf unused
	cd tables && mv *tex unused 

clean:
	rm *.aux *.log *.out *.bbl *.blg *.dvi *.aux *.fff *.lof *.out 

toread: $(project_name).pdf
	scp $(project_name).pdf bandit-local.local:~/Dropbox/toread/
