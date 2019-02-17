#' Function to load packages already installed otherwise install then load, may require rJava if not already installed
#'
#' @param pkg name of packages as a vector
#' @return check if packages are already installed, install necessary packages and load them
#' @import plyr ggplot2 tidyverse lubridate data.table openxlsx stringr dataPreparation corrplot caret e1071 rmarkdown gridExtra bit64
#' @export
install_packages <- function(pkg){
	new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
	if(length(new.pkg)) {
			install.packages(new.pkg, dependencies = TRUE)
		}
	sapply(pkg, require, character.only = TRUE)
}
tidy_packages <- function(pkg){
	easypackages::suppressWarnings(suppressMessages(install_packages(pkg)))
}

#' Function to load a file: replaces blank space by NA using na.string and check if there are duplicate column names
#'
#' @param file name of file along with its location
#' @return loads the data as data.table
#' @export
load_datatable <- function( file, check.names = TRUE) {
	data.table::fread(file = file,
										na.strings = c("", "NA"),
										check.names = check.names)
}

#' Function to load excel file: check if there are duplicate column names and find columns that are dates
#'
#' @param file name of excel file along with its location
#' @return loads the data as excel
#' @export
load_excel <- function( file, check.names = FALSE ) {
	openxlsx::read.xlsx(file, check.names = check.names, detectDates = TRUE)
}

#' Function saves the data, useful after data wrangling and manipulation
#'
#' @param data write the name of data
#' @param file_location write the location where to save the data and in which format
#' @return saves data as the format defined in file_location
#' @export
save_data <- function(data, file_location){
	data.table::fwrite(data, file = file_location, quote = TRUE, row.names = FALSE)
}

