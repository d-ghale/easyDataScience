df1 <- load_datatable("~/easyDataScience/tests/df1.csv", check.names = FALSE)
df1 <- dplyr::distinct( df1 )

test_df1 <- load_datatable("~/easyDataScience/tests/test_df1.csv", check.names = FALSE)
test_df1 <- remove_duplicate_rows(test_df1) 

test_that("check if duplicate rows are removed", {
	expect_equal(nrow(df1), nrow(test_df1))
})