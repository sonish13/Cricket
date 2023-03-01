## Cricket

Firstly, the data obtained from GitHub is contained in two CSV files,
mainly `IPL Ball-by-BAll 2008-2020.csv` and IPL Matches `2008-2020.csv`.
Both of these datasets were obtained [from
Kaggle](https://www.kaggle.com/datasets/patrickb1912/ipl-complete-dataset-20082020).

All the variables that we needed were calculated from these two csv
files.

The codes for variable calculations are in `Data Cleaning.R` file.

Output from data cleaning are in `First.csv` and `Second.csv` files
which contain ball-by-ball data for first and second innings
respectively.

`Selection.R` contains the codes used in selecting the important
covariates using AIC, BIC and CVd methods.

`First_Accuracy.Rmd` and `Second_Accuracy.Rmd` contains the code used to
compute the accuracy of the model using leave-one-out (LOO) cross
validation (CV).

`From AIC.Rmd` and `From BIC and CV.Rmd` contain code to calculate
accuracy on LOO-CV obtained from the model using variables obtained from
AIC, BIC and CV. (BIC and CV gave the same variables.) These scripts
have errors in them and need some work.

`Cricket.Rmd` creates `Cricket.html`, a simple description of work that
has been done.

`helper.R` has helper functions used throughout the project.
