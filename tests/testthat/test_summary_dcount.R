context("Summary")

my_df1 <- load_datatable("~/easyDataScience/tests/df1.csv", check.names = FALSE)
my_df2 <- load_datatable("~/easyDataScience/tests/df2.csv", check.names = TRUE)

t(sapply(1:ncol(my_df1), function(i){
	data.frame(Varaible = names(my_df1[,..i]), count_distinct = dplyr::n_distinct(my_df1[,..i]))
}))

summary1 <- summary_dcount(my_df1)
summary2 <- summary_dcount(my_df2)