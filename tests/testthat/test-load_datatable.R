context("Data")

site <- c("A", "B", "NA", NA, "E", "B")
D01 <- c(1, 0, 0, 0, 1, 0)
D01 <- c(1, 1, 0, 1, 1, 1)
D02 <- c(1, 0, 1, 0, 1, 0)
D02 <- c(0, 1, 0, 0, 1, 1)
D03 <- c(1, 1, 0, 0, 0, 1)
D03 <- c(0, 1, 0, 0, 1, 1)
D04 <- c(NA, NA, 0, 1, 1, NA)
D04 <- c(NA, 0, NA, 1, 1, 0)

df1 <- data.table(site, D01, D01, D02, D02, D03, D03, D04, D04, check.names = FALSE)
df2 <- data.table(site, D01, D01, D02, D02, D03, D03, D04, D04, check.names = TRUE)

# write.csv(df1, file = "~/easyDataScience/tests/df1.csv", row.names = FALSE)
# write.csv(df2, file = "~/easyDataScience/tests/df2.csv", row.names = FALSE)

my_df1 <- load_datatable("~/easyDataScience/tests/df1.csv", check.names = FALSE)
my_df2 <- load_datatable("~/easyDataScience/tests/df2.csv", check.names = TRUE)

test_that("check column names of the data", {
	expect_equal(colnames(my_df1), colnames(df1))
	expect_equal(colnames(my_df2), colnames(df2))
})
