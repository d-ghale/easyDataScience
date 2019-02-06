#' Function to print tidy version of "summary" function
#' This function converts input temperatures in Fahrenheit to Kelvin.
#' @param summary_dcount gives distinct numbers
#' @param summary_numbers gives summary of numeric variables
#' @param summary_dates gives summary of date variables
#' @param join_summary uses summary_numbers and summary_dates to give a tidy summary
#' @return distinct numbers, and basic statistics for each variable in the data
#' @export
#' @examples
#' tidy_summary(data)
#'
#'
#' Load necessary packages
#' This function loads packages without warnings 
#' @param pkg name of packages as a vector 
#' @param join_summary uses summary_numbers and summary_dates to give a tidy summary
#' @return distinct numbers, and basic statistics for each variable in the data
#' @export
#' @examples
#' tidy_summary(data)
#'
#'
install_packages <- function(pkg){
	# check if packages are already installed 
	new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
	# load the packges 
	if (length(new.pkg))
		install.packages(new.pkg, dependencies = TRUE)
	sapply(pkg, require, character.only = TRUE)
}

tidy_packages <- function(pkg){
	suppressWarnings(suppressMessages(install_packages(pkg)))
}


# usage
packages <- c("plyr", "dplyr", "readr", "tidyr", "ggplot2",	"data.table", "lubridate",	"dataPreparation", "corrplot",
							"ROCR", "pROC",	"randomForest",	"caret",	"e1071", "randomForestExplainer", "rmarkdown", "gridExtra",
							"reprtree", "IRdisplay")

tidy_packages(packages)

#' install.packages(c("devtools", "roxygen2"))
#' tidy_packages(c("devtools", "roxygen2"))
#' roxygen2 package provides an in-source documentation system that automatically generates R documentation (Rd) files
#' setwd(parentDirectory)
#' devtools::create("easyDataScience")
#' redo document() to update as needed 
#' setwd("./easyDataScience")
#' Run devtools::document() to convert roxygen comments to .Rd files.
#' setwd("..")
#' install("easyDataScience")
#' easyDataScience environment that is the parent environment to the global environment.
#' search()

#' Load necessary packages
#' @param pkg name of packages as a vector 
#' @param join_summary uses summary_numbers and summary_dates to give a tidy summary
#' @return distinct numbers, and basic statistics for each variable in the data
#' @export
#' @examples
#' tidy_summary(data)
#'
#'
#' Function to load a file: replaces blank space by NA using na.string and check if there are duplicate column names 
load_datatable <- function( file ) {
	fread(file = file,
				na.strings = c("", "NA"),
				check.names = TRUE)
}

#' The first thing we do is calculate basic statistics like mean, median, max to get a sense of what data we are looking at.
#' summary() function is only useful for numeric and logical type variables
#' cannot compute mean, median, etc, on character and logical types
#' also the functions' output is not in tidy format
#' transform each variable as a row and each type of statistics as a column with its corresponding value
#' + For numbers calculate summary statistics such as min, max and mean to understand how spread out the data are around the average, how large the sample is, etc. This also helps us to quickly identify errors (e.g. negative values) and missing values.
#' + For character or text variables, find the unique values

#' count the number of unique values in each variable
summary_dcount <- function(data) {
	dcount_func <- function(x){
		data.frame(count_distinct = n_distinct(x))
	}
	summary_data <- adply(data, 2, dcount_func )
	names(summary_data)[1] <- "Variable"
	data.frame(lapply(summary_data, trimws), stringsAsFactors = FALSE)
}

#' compute basic statistics for date types
#'
#' first of all find all of the columns that are dates
#' r doesn't have a simple function for that so we will build our own function
#' 
is_date <- function(x) {
	require(lubridate)
	if (class(x) == "data.frame")	warning("x is not a array")
	!all(is.na(as.Date(as.character(x), format = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y"))))
}

summary_dates <- function(data){
	date_data <- data[, lapply(data, is_date) == TRUE, with = FALSE]
	if (nrow(date_data) > 0){
		cols <- colnames(date_data)
		#' covert all of the dates into one format for consistency
		setColAsDate(date_data, cols = cols, format = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d"))
		summary_table <- summary(date_data)
		#' We can't use ":" to separate like in summary_numbers() because of %H:%M:%S in the column
		#' Therefore we are using the regular expression to split using the first occurance of ":" and keeping the remaining values
		data.frame(summary_table) %>%
			separate(Freq, c("type", "value"), "\\:(?=\\d)", extra = "merge") %>%
			select(- Var1)
	} else {
		date_data
	}
}

#' compute basic statistics for number types
summary_numbers <- function(data) {
	numeric_data <- data[, lapply(data, is.numeric) == TRUE, with = FALSE]
	summary_table <- data.frame(summary(numeric_data))
	summary_table <- summary_table  %>%
		separate(Freq, c("type", "value"), ":") %>%
		select(- Var1)
	summary_table$value <- round(as.numeric(summary_table$value), 2)
	summary_table
}

#' combine both dates and numbers
join_summary <- function(data){
	summary_data <- rbind(summary_dates(data), summary_numbers(data)) %>%
		spread(type, value)
	summary_data <- summary_data %>% replace(., is.na(.), "-")
	names(summary_data)[1] <- "Variable"
	#' string with . at end gives odd results when removing whitespace
	summary_data <- data.frame(lapply(summary_data, trimws), stringsAsFactors = FALSE)
	names(summary_data) <- gsub("[.]", "", names(summary_data))
	#' Order some of the columns
	setnames(summary_data, old = c("X1stQu", "X3rdQu"), new = c( "First.Qu", "Third.Qu"))
	col_order <- c("Variable", "Min", "First.Qu",  "Mean", "Median", "Third.Qu", "Max")
	setcolorder(summary_data, c(col_order, setdiff(names(summary_data), col_order)))
	summary_data
}

#' count the number of TRUE and FALSE for logical type
summary_logicals <- function(data){
	logical_data <- data[, lapply(data, is.logical) == TRUE, with = FALSE]
	if (nrow(logical_data) > 0){
		summary_data <- data.frame(summary(logical_data)) %>%
			select(-Var1) %>%
			separate(Freq, c("type", "value"), ":")
		summary_data$type <- trimws(summary_data$type)
		summary_data <- summary_data %>%
			spread(type, value) %>%
			select(- Mode)
		names(summary_data)[1] <- "Variable"
		data.frame(lapply(summary_data, trimws), stringsAsFactors = FALSE)
	} else {
		logical_data
	}
}

handle_name <- function(data){
	#' find colnames with "NA" in it
	col_names <- colnames(data)[grepl("NA", colnames(data)) == TRUE]
	colnames(data)[which(colnames(data) %in%  col_names)] <- "None"
	None_data <- data.frame(data, check.names = TRUE) #' don't allow duplicate names
	col_names2 <- colnames(None_data)[grepl("None", colnames(None_data)) == TRUE]
	cols_join <- None_data[, names(None_data) %in% col_names2]
	if(length(col_names) > 1){
		None <- as.vector(do.call(coalesce, cols_join))
		cbind(None_data[, - which(names(None_data) %in% col_names2)], None)
	} else {
		None_data
	}
}

tidy_summary <- function(data){
	dcount_data <- data.frame(summary_dcount(copy(data)))
	numbers_data <- data.frame(handle_name(join_summary(copy(data))))
	logicals_data <- data.frame(handle_name(summary_logicals(copy(data))))
	if (nrow(logicals_data) == 0 & nrow(numbers_data) == 0) {
		full_summary <- dcount_data
	} else if (nrow(logicals_data) == 0) {
		full_summary <- merge(dcount_data, numbers_data, by = "Variable", all = TRUE)
	} else {
		full_summary <- if ("None" %in% colnames(numbers_data) && "None" %in% colnames(logicals_data) ) {
			merge(dcount_data, merge(logicals_data, numbers_data, by = c("Variable", "None"), all = TRUE) , by = "Variable", all = TRUE)
		} else {
			merge(dcount_data, merge(logicals_data, numbers_data, by = "Variable", all = TRUE) , by = "Variable", all = TRUE)
		}
	}
	# characters and logical data types don't have min, median, etc. which can be replaced with a dash "-" instead of NAs
	full_summary  %>% 
		replace(., is.na(.), "-")
}

#' Function that removes any duplicate rows
remove_duplicate_rows <- function( data ) {
	distinct( data )
}

#' Function that prints distinct values based on one variable
#' @param data name of the dataframe/datatable
#' @param ... a column name or a vector with multiple names
#
unique_keys_one <- function(data, ...) {
	distinct_(data[,...]) %>%
		pull(...) #' print as a vector instead of a column format
}

#' Function that prints distinct values based on more than one group
#' #' @param data name of the dataframe/datatable
#' @param ... a column name or a vector with multiple names
#' @example  unique_keys_more(df, c("col1", "col2"))
#' 
unique_keys_more <- function(data, ...) {
	distinct_(data[,...]) #' print as column format
}

#' We must impute the missing data in a variable 
#' We can remove the entire row (avoid if relatively small data size), replace with either 0 or mean/median value of all the available variable
#' It is okay to replace NA with 0 when counting the values

#' replace NAs with 0 for integers and replace NAs with "missing" in character columns
handle_NA <- function( data ) {
	dataPreparation::fastHandleNa(data, set_num = 0,
		set_logical = FALSE,
		set_char = "missing")
	data
}


#' we need another function to replace "NA" because NA as string is not treated as NA
replace_NA <- function( data ) {
	for ( k in names( data ) )
		if( is.character( data[[ k ]] ) )
			set(data,
				i = which( data[[ k ]] == "NA" ) ,
				j = k,
				value = "missing" )
	data
}

encoding_col <- function( data, column) {
	encoding <<- build_encoding(data, cols = column )
	one_hot_encoder(data, encoding = encoding,
		type = "logical", #' avoid possible confusion as number
		drop = TRUE)
	data
}

#' Correlation can be evaluated only for numeric type
#' Plot the correlationship
plot_rcor <- function( data ) {
	numeric_data <- data[, lapply(data, is.numeric) == TRUE, with = FALSE]
	var_cor <- rcorr( as.matrix( numeric_data ) )
	corrplot( var_cor$r, type = "lower", order="hclust", p.mat = var_cor$P, sig.level = 0.01, insig = "blank")
	#' Insignificant correlations are leaved blank
}

convert_all_numeric <- function( data ) {
	col_numeric <- data
	col_numeric[, names( col_numeric ) := lapply(.SD, as.numeric ) ]
}


factor_func <- function(data, ...){
	factor_cols <- data[, list(...)]
	for ( k in seq_along( factor_cols ) ) 
		set(factor_cols, j = k, value = as.factor(as.character(factor_cols[[ k ]]))) 
	data <- data[, names( factor_cols ) := factor_cols ]
	data <- factor_is_rejected(data)
	data
}

#' Standarize all the independent variables (convert the mean into 0 with standard deviation 1)
#' using `scale` function, which by default does z-score transformation

z_score_transform <- function(data, ...){
	keep_cols <- colnames(data[, ...])
	data[, (keep_cols) := lapply(.SD, scale), .SDcols = keep_cols]
}

#' All combinations of variables with the dependent variable
#' keep the dependent variable as the first column
single_combn <- function(data) {
	vec <- names(data)
	pos <- 1
	as.data.frame(rbind(vec[pos], vec[-pos]))
}

trans_vars_func <- function(data){
	t_vars_data <- transpose(single_combn(data))
	#' get row and colnames in order
	colnames(t_vars_data) <- c("output", "input")
	t_vars_data %>%
		mutate(formula = paste(t_vars_data$output, "~", t_vars_data$input))
}

make_formula <- function(data, index){
	as.formula(data[eval(index), 3])
}

#' write a logistic regression model
make_formula_1var <- function(data){
	vars_data <- trans_vars_func(data)
	lst <- c()
	n_predictors <- ncol(data) - 1
	for (i in 1:n_predictors){
		lst[[i]] <- as.formula(vars_data[eval(i), 3])
	}
	lst
}

glm_model_1var <- function(data){
	formula_lst <- make_formula_1var(data)
	output_lst <- list()
	for (i in 1:length(formula_lst)){
		output_lst[[i]] <- summary(glm(eval(formula_lst[[i]]), data = eval(data), family = binomial ))$coefficients
	}
	output_lst
}

glm_model <- function(formula){
	summary(glm(eval(formula), data = eval(data), family = binomial ))$coefficients
}
#' Logistic regression coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variable.
odds_coef <- function(model){
	coef_predictor <- coef(model)
	#' log odds to odds
	odds_predictor <- exp(coef_predictor)
	#' confidence interval of predictors
	confint_odds_predictor <- exp(confint(model))
	#' odds ratios and 95% CI using standard errors
	cbind(OddsRatio = round(odds_predictor, 4), round(confint_odds_predictor, 4))
}

log_odds_prob <- function(model){
	coef_predictor <- coef(model)
	#' log odds to odds
	odds_predictor <- exp(coef_predictor)
	#' confidence interval of predictors
	confint_odds_predictor <- exp(confint(model))
	#' probability and 95% CI using standard errors
	cbind(Probability = round(odds_predictor/(1 + odds_predictor), 4), round(confint_odds_predictor/(1 + confint_odds_predictor), 4))
}

rf_code <- getModelInfo("rf", regex = FALSE)[[1]]

#' we are trying to solve a classification problem
rf_code$type <- c("Classification")

#' add two parameters: number of trees and threshold
rf_code$parameters <- data.frame(parameter = c("mtry", "ntree", "threshold"),
	class = rep("numeric", 3),
	label = c("No. Randomly Selected Predictors", "No. Trees", "Probability Cutoff"))
rf_code$grid <- function(x, y, len = NULL, search = "grid") {}
rf_code$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
	randomForest(x, y,
		mtry = param$mtry,
		ntree = param$ntree,
		threshold = param$threshold, ...)
}

#' This function saves the data 
#' @param data write the name of data 
#' @param file_location write the location where to save the data and in which format 
#' @return saves data as the format defined in file_location
#' @export
#' @examples save the data after data wrangling and manipulation: '~/Documents/[folders]/[file_name.csv]' 
#' 
save_data <- function(data, file_location){
	fwrite(data, file = file_location, quote = TRUE, row.names = FALSE)
} 

# how to publish data


