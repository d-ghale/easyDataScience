context("Packages")

# pkg <- "dplyr"

suppressMessages(install.packages(pkg))
quietly_packages(pkg, suppress = "messages")

suppressWarnings(install.packages(pkg))
quietly_packages(pkg, suppress = "warnings")

suppressWarnings(suppressMessages(install.packages(pkg)))
quietly_packages(pkg)

test_that("check if packages loads correctly", {
	expect_equal(suppressMessages(install.packages(pkg)), quietly_packages(pkg, suppress = "messages"))
	expect_equal(suppressWarnings(install.packages(pkg)), quietly_packages(pkg, suppress = "warnings"))
	expect_equal(suppressWarnings(suppressMessages(install.packages(pkg))), quietly_packages(pkg))
})
