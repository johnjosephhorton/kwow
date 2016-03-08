raw/mturk_output.csv:
	curl -o raw/mturk_output.csv -L https://www.dropbox.com/s/y9uebbcq08q841g/mturk_output.csv?dl=1

raw/national_M2012_dl.csv:
	curl -o raw/national_M2012_dl.csv -L https://www.dropbox.com/s/lzkt7pdyfnx6oo4/national_M2012_dl.csv?dl=1

raw/education.categories.csv:
	curl -o raw/education.categories.csv -L https://www.dropbox.com/s/bxc7hagjqlv7dfy/education.categories.csv?dl=1

raw/occupation.table.1.2.csv:
	curl -o raw/occupation.table.1.2.csv -L https://www.dropbox.com/s/n3fbjjgi6kng5vl/occupation.table.1.2.csv?dl=1

transformed/mturk_cooked.rds: raw/mturk_output.csv raw/national_M2012_dl.csv raw/education.categories.csv raw/occupation.table.1.2.csv
	./construct_datasets.R

../analysis/kwowR/inst/extdata/mturk_cooked.rds: transformed/mturk_cooked.rds
	cp transformed/mturk_cooked.rds ../analysis/kwowR/inst/extdata/

targets = ../analysis/kwowR/inst/extdata/mturk_cooked.rds

data : $(targets)
