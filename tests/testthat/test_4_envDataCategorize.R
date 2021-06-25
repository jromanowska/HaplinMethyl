context("Testing categorization of environmental data")

source("common_vars.R")

examples.dir <- system.file("extdata", package = "HaplinMethyl")
test.read.in <- envDataRead(my.env.data.all.names.in,
	dir.in = examples.dir,
	file.out = my.env.data.all.names.out,
	sep = " ",
	cont = TRUE,
	header = TRUE,
	rownames = TRUE,
	overwrite = TRUE)

test_that("Wrong input data type", {
	expect_error(envDataCategorize(1:3, overwrite = TRUE))
})

test_that("'breaks' given as a number - 1", {
	expect_error(envDataCategorize(test.read.in, breaks = 1, overwrite = TRUE))
})

test_that("'breaks' given as a number - less than 1", {
	expect_error(envDataCategorize(test.read.in, breaks = 0, overwrite = TRUE))
})

test_that("'breaks' given as a number - 4", {
	breaks <- 4
	env.data.cat <- envDataCategorize(test.read.in,
									   breaks = breaks,
									   overwrite = TRUE)
	expect_s3_class(env.data.cat, get(".class.data.env.cat",
								   envir = .haplinMethEnv))
	expect_equal(length(env.data.cat), length(test.read.in))
	expect_equal(levels(env.data.cat[[ 1 ]]), as.character(1:breaks))
})

test_that("'breaks' given as a vector", {
	breaks <- c(0,0.25,0.5,0.75,1)
	env.data.cat <- envDataCategorize(test.read.in,
									   breaks = breaks,
									   overwrite = TRUE)
	expect_s3_class(env.data.cat, get(".class.data.env.cat",
								   envir = .haplinMethEnv))
	expect_equal(length(env.data.cat), length(test.read.in))
	expect_equal(levels(env.data.cat[[ 1 ]]), as.character(1:(length(breaks) - 1)))
})
