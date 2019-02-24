context("Handling NAs")

df1 <- load_datatable("~/easyDataScience/tests/df1.csv")
df1 <- dataPreparation::fastHandleNa(df1, set_num = 0) 

test_df1 <- load_datatable("~/easyDataScience/tests/test_df1.csv")
test_df1 <- handle_NA(test_df1, set_char = "") 

df2 <- load_datatable("~/easyDataScience/tests/df2.csv")
df2 <- dataPreparation::fastHandleNa(df2, set_num = 0) 

test_df2 <- load_datatable("~/easyDataScience/tests/test_df2.csv")
test_df2 <- handle_NA(test_df2, set_char = "") 

df2_1 <- load_datatable("~/easyDataScience/tests/df2.csv")
df2_1 <- dataPreparation::fastHandleNa(df2_1, set_num = 0, set_char = "missing") 

test_df2_1 <- load_datatable("~/easyDataScience/tests/test_df2.csv")
test_df2_1 <- dataPreparation::fastHandleNa(test_df2_1, set_num = 0, set_char = "missing") 

df2_2 <- copy(df2_1)
df2_2[df2_2 == "NA"] <- "missing"

test_df2_2 <- replace_NA(copy(test_df2_1))

test_that("check if NAs are replaced by 0 for numeric type and by 'missing' for character type", {
	expect_equal(df1, test_df1)
	expect_equal(df2, test_df2)
	expect_equal(df2_1, test_df2_1)
	expect_equal(df2_2, test_df2_2)
})
