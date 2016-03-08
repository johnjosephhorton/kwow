# Notes

This is the associated code for the paper ``Good on Wages, Bad on Changes'' 

1. My website: [http://www.john-joseph-horton.com/papers/sharing.pdf](http://www.john-joseph-horton.com/papers/sharing.pdf)

## Citation Info

```
@article{horton2016kwow,
 title = "Good on Wages, Bad on Changes",
 author = "John J. Horton and Adam Kapelner",
 year = "2016",
 month = "February",
 journal = "Working Paper" 
}	  

```

## Replication

The repository is set up to make it transparent how the final PDF is constructed from the raw data. 
To replicate, you will need a Linux or Mac OX machine that has the following installed:

1. `R`
1. `pdflatex`
1. `make`
1. `gpg`
1. `curl`
1. `gs` (GhostScript)

To replicate the data analysis, you will need several R packages.
However, when you run the code below, it *should* obtain all these R-specific dependencies you need. 

Note that this repository does not contain the actual experimental data.
To obtain the data, email me at `john.joseph.horton@gmail.com` and I will email you two small text files.
These files have the information you need to download and unencrypt the data. 

One you have the two files, the steps are:

####Download the repository from github
```
 git clone git@github.com:johnjosephhorton/kwow.git 
```
#### Download the data

```
cd etl
make data
```

#### Run the analsysis

#### Build the PDF
From `/kwow`, run: 
```
cd writeup
make kwow.pdf
```
This should download the necessary data files and decrypt them.
It will also run the statistical analysis in R (downloading all needed packages) and then produce plots and tables (stored in `writeup/tables` and `writeup/plots`). 
Finally, it will build the pdf file using `pdflatex`, leaving the resultant `sharing.pdf` in the `/writing` folder.
To see the actual steps that are being followed, you can inspect `writeup\Makefile`.

If you run into any trouble replicating, please contact me at ``john.joseph.horton@gmail.com``. 

