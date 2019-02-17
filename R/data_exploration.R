#' Function count the number of unique values in each variable
#'
#' @param data name
#' @return count of distinct values for each variable in the data
#' @import plyr ggplot2 tidyverse lubridate data.table openxlsx stringr dataPreparation corrplot caret e1071 rmarkdown gridExtra bit64
#' @export
summary_dcount <- function(data) {
	dcount_func <- function(x){
		data.frame(count_distinct = n_distinct(x))
	}
	summary_data <- adply(data, 2, dcount_func )
	names(summary_data)[1] <- "Variable"
	data.frame(lapply(summary_data, trimws), stringsAsFactors = FALSE)
}

#' Function checks whether a variable in the data is of date type or not. r doesn't have a simple function for that so we will build our own function
#'
#' @param data name
#' @return  TRUE or FALSE whether variable is of date type
#' @export
is_date <- function(x) {
	if (class(x) == "data.frame")	warning("x is not a array")
	!all(is.na(as.Date(as.character(x), format = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y"))))
}

#' Function compute basic statistics for date types
#'
#' @param data name
#' @return  TRUE or FALSE whether variable is of date type
#' @export
summary_dates <- function(data){
	is_date <- function(x) {
		if (class(x) == "data.frame")	warning("x is not a array")
		!all(is.na(as.Date(as.character(x), format = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y"))))
	}
	#' find all of the columns that are dates
	date_data <- data[, lapply(data, is_date) == TRUE, with = FALSE]
	if (nrow(date_data) > 0) {
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

#' Function compute basic statistics for numeric types
#'
#' @param data name
#' @return  TRUE or FALSE whether variable is of date type
#' @export
summary_numbers <- function(data) {
	numeric_data <- data[, lapply(data, is.numeric) == TRUE, with = FALSE]
	summary_table <- data.frame(summary(numeric_data))
	summary_table <- summary_table  %>%
		separate(Freq, c("type", "value"), ":") %>%
		select(- Var1)
	summary_table$value <- round(as.numeric(summary_table$value), 2)
	summary_table
}

#' Function combines the summary of date and numeric variables
#'
#' @param data name
#' @export
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

#' Function counts the number of TRUE and FALSE for logical type
#'
#' @param data name
#' @export
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

#' Function renames the column so that values such as NA are avoided, find colnames with "NA" in it, don't allow duplicate names
#'
#' @param data name
#' @export
handle_name <- function(data){
	col_names <- colnames(data)[grepl("NA", colnames(data)) == TRUE]
	colnames(data)[which(colnames(data) %in%  col_names)] <- "None"
	None_data <- data.frame(data, check.names = TRUE)
	col_names2 <- colnames(None_data)[grepl("None", colnames(None_data)) == TRUE]
	cols_join <- None_data[, names(None_data) %in% col_names2]
	if(length(col_names) > 1){
		None <- as.vector(do.call(coalesce, cols_join))
		cbind(None_data[, - which(names(None_data) %in% col_names2)], None)
	} else {
		None_data
	}
}

#' Function to print tidy version of "summary" function
#'
#' @param data name
#' @return distinct numbers, and basic statistics for each variable in the data. characters and logical data types don't have min, median, etc. which can be replaced with a dash "-" instead of NAs
#' @export
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
	full_summary  %>%
		replace(., is.na(.), "-")
}

#' Function that prints distinct values based on one variable
#'
#' @param data name of the dataframe/datatable
#' @param ... a column name or a vector with multiple names
#' @export
unique_keys_one <- function(data, ...) {
	distinct_(data[,...]) %>%
		pull(...) #' print as a vector instead of a column format
}

#' Function that prints distinct values based on more than one group
#'
#' @param data name of the dataframe/datatable
#' @param ... a column name or a vector with multiple names
#' @export
unique_keys_more <- function(data, ...) {
	distinct_(data[,...]) #' print as column format
}

#' Function that counts number of observation of key based on group/s
#'
#' @param data name of the dataframe/datatable
#' @param ... a column name or a vector with multiple names
#' @export
count_keys_per_group <- function(data, col_key, ...) {
	new_name <- paste0("n_", col_key, collapse = "")
	data[, new_name := length(col_key), by = .(...) ]
}

#' Function that counts number of rows and percentage based on group/s
#'
#' @param data name of the dataframe/datatable
#' @param ... a column name or a vector with multiple names
#' @export
count_per_group <- function(data, ...){
	data %>%
		group_by(...) %>%
		summarise(n = n(), percentage = round((n*100)/nrow(data), 2))
}
