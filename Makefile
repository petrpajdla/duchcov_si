all: si/duchcov_support.html plots/FIG5.pdf plots/FIG6.pdf plots/FIG7_1.pdf plots/FIG7_2.pdf plots/FIG8.pdf plots/FIG9.pdf plots/FIG10.pdf

si/duchcov_support.html: si/duchcov_references.bib si/duchcov_support.Rmd data/duchcov.csv data/ore_sources.csv data/burial_grounds.csv
	cd code; R CMD BATCH knit_report.R

plots/FIG5.pdf: code/figure5.R data/duchcov.csv
	cd code;R CMD BATCH figure5.R
    
plots/FIG6.pdf: code/figure6.R data/duchcov.csv
	cd code;R CMD BATCH figure6.R

plots/FIG7_1.pdf: code/figure7.R data/duchcov.csv data/ore_sources.csv
	cd code;R CMD BATCH figure7.R
    
plots/FIG7_2.pdf: code/figure7.R data/duchcov.csv data/ore_sources.csv
	cd code;R CMD BATCH figure7.R

plots/FIG8.pdf: code/figure8.R data/duchcov.csv data/ore_sources.csv
	cd code;R CMD BATCH figure8.R

plots/FIG9.pdf: code/figure9.R data/duchcov.csv data/burial_grounds.csv
	cd code;R CMD BATCH figure9.R

plots/FIG10.pdf: code/figure10.R data/duchcov.csv data/burial_grounds.csv
	cd code;R CMD BATCH figure10.R

.PHONY : clean
clean :
	cd code;rm Rplots.pdf .RData
