context("Factoring columns")

df1 <- load_datatable("~/easyDataScience/tests/df1.csv")
df1_1 <- copy(df1)

factor_cols1 <- df1_1[, list(D01, D02, D03)]
factor_cols2 <- df1_1[, c("D01", "D02", "D03")]
for ( k in seq_along( factor_cols ) )
	set(factor_cols, j = k, value = as.factor(as.character(factor_cols[[ k ]])))
data <- data[, names( factor_cols ) := factor_cols ]
data

test_df1 <- load_datatable("~/easyDataScience/tests/test_df1.csv")
test_df1_1 <- copy(test_df1)
test_df1_1 <- factor_func(test_df1_1, "D01", "D02", "D03")

test_that("check if NAs are replaced by 0 for numeric type and by 'missing' for character type", {
	expect_equal(factor_cols1, factor_cols2)
})
