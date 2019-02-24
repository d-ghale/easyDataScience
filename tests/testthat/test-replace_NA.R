context("Handling NAs")

df2 <- load_datatable("~/easyDataScience/tests/df2.csv")

test_df2 <- load_datatable("~/easyDataScience/tests/test_df2.csv")

df2_1 <- copy(df2)
df2_1[df2_1 == "NA"] <- "missing"

test_df2_1 <- replace_NA(copy(test_df2))

test_that("check if NAs are replaced by 0 for numeric type and by 'missing' for character type", {
	expect_equal(df2_1, test_df2_1)
})
