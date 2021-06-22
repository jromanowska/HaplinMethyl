context("Check extracting of raw data")

source("common_vars.R")

examples.dir <- system.file("extdata", package = "HaplinMethyl")
test.read.in <- envDataRead(
  my.env.data.all.names.in,
	dir.in = examples.dir,
	file.out = my.env.data.all.names.out,
	sep = " ",
	cont = TRUE,
	header = TRUE,
	rownames = TRUE,
	overwrite = TRUE
)

test_that("Wrong input data type", {
	expect_error(showRaw(1:3))
})

test_that("Wrong 'rows' argument", {
  expect_error(showRaw(test.read.in, rows = 0))
  expect_error(showRaw(test.read.in, rows = c()))
  expect_error(showRaw(test.read.in, rows = 1000))
  expect_error(showRaw(test.read.in, rows = "no_such_row"))
})

test_that("Wrong 'columns' argument", {
  expect_error(showRaw(test.read.in, columns = 0))
  expect_error(showRaw(test.read.in, columns = c()))
  expect_error(showRaw(test.read.in, columns = 1000))
  expect_error(showRaw(test.read.in, columns = "no_such_col"))
})

test_that("Default arguments", {
  out <- showRaw(test.read.in)
  summary_in <- summary(test.read.in, short = FALSE)
  expect_equal(
    object = colnames(out), expected = summary_in$colnames[1:5]
  )
  expect_equal(
    object = rownames(out), expected = summary_in$rownames[1:5]
  )
  expect_equal(
    object = as.vector(out),
    expected = as.vector(test.read.in[[1]][1:5, 1:5])
  )
})

test_that("Row order maintained", {
  row_order <- c(3, 5, 1, 6)
  out <- showRaw(test.read.in, rows = row_order)
  summary_in <- summary(test.read.in, short = FALSE)
  expect_equal(
    object = colnames(out), expected = summary_in$colnames[1:5]
  )
  expect_equal(
    object = rownames(out), expected = summary_in$rownames[row_order]
  )
  expect_equal(
    object = as.vector(out),
    expected = as.vector(test.read.in[[1]][row_order, 1:5])
  )
})

test_that("Columns given as character vector", {
  col_order <- c("cg2", "cg5", "cg1", "cg5")
  out <- showRaw(test.read.in, columns = col_order)
  summary_in <- summary(test.read.in, short = FALSE)
  expect_equal(
    object = colnames(out), expected = col_order
  )
  expect_equal(
    object = rownames(out), expected = summary_in$rownames[1:5]
  )
  expect_equal(
    object = as.vector(out),
    expected = as.vector(test.read.in[[1]][1:5, match(col_order, summary_in$colnames)])
  )
})
