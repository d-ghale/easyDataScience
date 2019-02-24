#' Function that removes any duplicate rows
#'
#' @param data name
#' @return data without duplicated rows
#' @export
remove_duplicate_rows <- function( data ) {
	dplyr::distinct( data )
}

#' Function replace NAs with 0 for integers and replace NAs with "missing" in character columns
#'
#' @param set_num replace NULL values in numeric type by set_num
#' @param set_logical replace NULL values in logical type by set_num
#' @param set_char replace NULL values in character type by set_num
#' @return data without NULL values
#' @export

handle_NA <- function( data, set_num = 0, set_logical = FALSE, set_char = "missing" ) {
	set_formula <- if(is.character(set_num) == TRUE) {
		paste0("function(x)", set_num, "(x, na.rm = TRUE)", collapse = "")
	} else {
		set_num
	}
	dataPreparation::fastHandleNa(data, set_num = eval(parse(text = set_formula)),
		set_logical = set_logical,
		set_char = set_char)
	data
}

#' Function to replace "NA" because NA as string is not treated as NA
#'
#' @param set_char replace NULL values in "NA" by set_char
#' @export
replace_NA <- function( data, set_char = "missing" ) {
	for ( k in names( data ) )
		if( is.character( data[[ k ]] ) )
			set(data,
				i = which( data[[ k ]] %in% c("NA", "") ) ,
				j = k,
				value = set_char )
	data
}

#' Function that converts all of the variables into numeric type
#' @export
convert_all_numeric <- function( data ) {
	col_numeric <- data
	col_numeric[, names( col_numeric ) := lapply(.SD, as.numeric ) ]
}

#' Function that converts all of the variables listed into factor type
#'
#' @param ... list of variables to factorize
#' @export
factor_func <- function(data, ...){
	factor_cols <- data[, ...]
	for ( k in seq_along( factor_cols ) )
		set(factor_cols, j = k, value = as.factor(as.character(factor_cols[[ k ]])))
	data <- data[, names( factor_cols ) := factor_cols ]
	data
}

#' Function standarizes all the independent variables (convert the mean into 0 with standard deviation 1) using scale() function, which by default does z-score transformation
#'
#' @param ... list of variables to factorize
#' @export
z_score_transform <- function(data, ...){
	keep_cols <- colnames(data[, ...])
	data[, (keep_cols) := lapply(.SD, scale), .SDcols = keep_cols]
}

#' Function that converts the given columns' values into individual column with TRUE or FALSE
#'
#' @param ... list of variables to encode as dummy variable
#' @return  one hot encode variables as logical values to avoid possible confusion as number
#' @export
encoding_col <- function( data, column) {
	encoding <<- dataPreparation::build_encoding(data, cols = column )
	dataPreparation::one_hot_encoder(data, encoding = encoding,
		type = "logical",
		drop = TRUE)
}

# lexicographic sorting compares character by character making 10 greater than 5
# we made the function below to solve the problem

leading_zero <- function(data, column){
	width <- max(nchar(as.character(data[, ..column])))
	data[, ..column] <- formatC(data[, ..column], width = width, flag = "0")
	data
}



