context("Packages")

pkg <- c("dplyr", "data.table")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) {
	utils::install.packages(new.pkg, dependencies = TRUE)
}
sapply(pkg, require, character.only = TRUE)

tidy_packages(pkg)