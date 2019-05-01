# easyDataScience

## Why I created this package?

This package is an attempt to perform data exploration performed commonly during the initial phase any data science project. 
The first thing we do when we get data is calculate basic statistics like mean, median, max to get a sense of what data we are looking at. summary() function is only useful for numeric and logical type variables since we cannot compute mean, median, etc. on character and logical variables. Also the functions' output is not in a tidy format. Therefore I built several functions to end up with a tidier summary of data (transform each variable as a row and each type of statistics as a column with its corresponding value). 

- For each of the variables in the data calculate the count of distinct values. This is helpful for character (text) variables. 

- For logical variables count the total number of TRUE and FALSE in the data. R's logical test function `is.logical()` treats columns with all NAs as logical. I removed those columns from my summary_logicals function. 

- For numbers calculate summary statistics such as min, max and mean to understand how spread out the data are around the average, how large the sample is, etc. This also helps us to quickly identify errors (e.g. negative values) and missing values.

- For dates first of all find the variables that are of date type. r doesn't have a simple function for that so we will build our own function. 

After performing basic statistics we need to impute missing variables. We can remove the entire row (avoid if relatively small data size), replace with either 0 or mean/median value of all the available variable. It is okay to replace NA with 0 when counting the values. For numeric variables, function `handle_NA` replaces NAs with 0 and for character variables the function replaces NAs with "missing". Sometimes we have NAs in string format "NA", which is not detected by base R's is.na function so we created another function (`replace_NA`) to replace "NA".  

`dataPreparation` package has many useful functions. For an example `one_hot_encoder` function encodes a column into dummy variable 0 or 1 which is detected as numbers as opposed to logical value. When you are dealing with large number of columns adding another function to convert each of those dummy variables into logical type is time consuming. Therefore I requested the author to add a parameter type = "logical" so that we can create those dummy variables as TRUE or FALSE. This is helpful in some cases and not in other but it is always good to have that option. 

Correlation can be evaluated only for numeric type so using `rcorr` and `corrplot` functions from `corrplot` package gives error when you pass data with non-numeric variables. So, I added a function `plot_rcor` that first grabs columns that are numeric types then uses the functions to plot the correlationship between those numeric variables.  
If data has all numeric variables, but they are stored as character variables then use `convert_all_numeric` to convert all of the variables in a data to numeric type. 

If you want to convert some of the variables into factor type, in `factor_func` pass the data as first argument then a list of variables/columns. 

We might want to scale our numeric independent variables in a standard format. One way to do that is use convert the mean into 0 with standard deviation 1, aka z-score transformation. Pass the data and a list of column names into `z_score_transform` function. 

Deciding which of the variables to use in our logistic regression model is also a part of data exploration. It is not a good idea to use all of the variables to build the model and use p-value test to decide significance of a variable as we know that our model is affected by multi-colinearity. To decide which of the multi-collinear variables to discard, we should run a model for each of the independent variables. We can then use p-value test to decide which variables to not use in our intial model. Rule of thumb is 

## How I created this package? 

Step 1: Install "devtools" and "roxygen2" (roxygen2 provides an in-source documentation system that automatically generates R documentation files), and load them
`install.packages(c("devtools", "roxygen2"))`
`library("devtools", "roxygen2")`

Step 2: Set a working directory (replace "parentDirectory") where you want to store your package  
`setwd(parentDirectory)`

Step 3: Create the package name (replace "easyDataScience")
`devtools::create("easyDataScience")`

Step 4: Set the package created in Step3 as working directory 
`setwd("./easyDataScience")`

Step 5: Create a folder named "R" and add your Rscript files, which contain functions you want to make available in the package

Step 6: Test the document
`check(document = FALSE)`

Step 7: Convert roxygen comments to .Rd files.
`devtools::document()`

Step 8: Clear your Console and try installing your package  
`install("easyDataScience")`

Step 9: If you update Rscript run the code below to make the changes available  
`document()`
