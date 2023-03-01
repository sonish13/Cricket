# Cricket

# Firstly, the data obtained from github is contained in two csv files mainly IPL Ball-by-BAll 2008-2020.csv and IPL Matches 2008-2020.csv

# All the variables that we needed were calcculated from this two csv file.
# The codes for variable calculations are in Data Cleaning.R file.
# Output from data cleaning are in First.csv and Second.csv files which contain ball-by-ball data for first and second innings respectively.
# Selection.R contains the codes used in selecting the important covariates using, AIC, BIC and CVd methods.
$ First_Accuracy.Rmd and Second_Accuracy.Rmd contans he codes used to compute the accuracy of the model using Leave-One-Out Cross validation.
# From AIC.Rmd and From BIC and CV .Rmd contains codes to calculate accuracy on LOO Cross validation obtained from model using variables obtained 
from AIC, BIC and CV. (BIC and CV gave the same variables). These codes have error in thm and needs some work.
# Cricket.Rmd and and Cricket.html is a sample slides of works that have been done.
# helper.R has helper functions used throughout the project.

