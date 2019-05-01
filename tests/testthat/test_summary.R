context("Summary")

my_df1 <- load_datatable("~/easyDataScience/tests/df1.csv", check.names = FALSE)
my_df2 <- load_datatable("~/easyDataScience/tests/df2.csv", check.names = TRUE)

summary(my_df1)
summary(my_df2)

tidy_summary(my_df1)
tidy_summary(my_df2)
