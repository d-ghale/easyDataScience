context("Factoring columns")

df1 <- load_datatable("~/easyDataScience/tests/df1.csv")
df1_1 <- copy(df1)

factor_cols <- df1_1[, list(D01, D02, D03)]
# same as factor_cols2 <- df1_1[, c("D01", "D02", "D03")]

for ( k in seq_along( factor_cols ) )
	set(factor_cols, j = k, value = as.factor(as.character(factor_cols[[ k ]])))
df1_1 <- df1_1[, names( factor_cols ) := factor_cols ]


factor_func <- function(data, ...){
	factor_cols <- colnames(data[, ...])
	set(data)[, (factor_cols) := lapply(.SD, factor), .SDcols = factor_cols]
	data
}
#setDT(data)[, (cols):= lapply(.SD, factor), .SDcols = cols]

df1_2 <- copy(df1)
df1_2 <- factor_func(df1_2, c("D01", "D02", "D03"))
df1_2 <- factor_func(df1_2, list(D01, D02, D03))


test_that("check if NAs are replaced by 0 for numeric type and by 'missing' for character type", {
	expect_equal(df1_1, df1_2)
})
