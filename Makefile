all: si/duchcov_support.html

si/duchcov_support.html:
	cd code; R CMD BATCH knit_report.R
