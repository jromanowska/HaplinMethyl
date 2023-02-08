context( "Testing envDataSubset function." )

source( "common_vars.R" )

examples.dir <- system.file( "extdata", package = "HaplinMethyl" )
test.read.in <- envDataRead( my.env.data.all.names.in,
	dir.in = examples.dir,
	file.out = my.env.data.all.names.out,
	sep = " ",
	cont = TRUE,
	header = TRUE,
	rownames = TRUE,
	overwrite = TRUE )
test.read.row.names.in 	<- envDataRead( my.env.data.row.names.in,
	dir.in = examples.dir,
	file.out = my.env.data.row.names.out,
	sep = " ",
	cont = TRUE,
	header = FALSE,
	rownames = TRUE,
	overwrite = TRUE )

subset.seq <- c( 3, 5, 10, 121 )

test_that( "No arguments provided", {
	expect_error( envDataSubset( test.read.in, overwrite = TRUE ) )
} )

test_that( "Wrong input format", {
	expect_error( envDataSubset( subset.seq ) )
})

test_that( "Duplicate arguments: columns", {
	expect_error( envDataSubset( test.read.in, overwrite = TRUE,
								col.ids = subset.seq, col.names = subset.seq ) )
})

# Subsetting columns only

test_that( "Subsetting only columns, by number", {
	cols.out <- subset.seq
	subset.out <- envDataSubset( test.read.in, col.ids = cols.out,
								 overwrite = TRUE )

	expect_s3_class( subset.out, class = "env.data" )
	expect_equal( colnames( subset.out[[ 1 ]] ),
				  paste0( "cg", cols.out ) )
	expect_equal( rownames( subset.out[[ 1 ]] ),
				  rownames( test.read.in[[ 1 ]] ) )
} )

test_that( "Subsetting only columns, by names", {
  cols.out <- paste0( "cg", subset.seq )
  subset.out <- envDataSubset( test.read.in, col.names = cols.out,
                               overwrite = TRUE )
  
  expect_s3_class( subset.out, class = "env.data" )
  expect_equal( colnames( subset.out[[ 1 ]] ),
                paste0( "cg", subset.seq ) )
  expect_equal( rownames( subset.out[[ 1 ]] ),
                rownames( test.read.in[[ 1 ]] ) )
} )

test_that( "Subsetting only columns, by names, wrong format", {
  cols.out <- subset.seq
  expect_error( envDataSubset( test.read.in, col.names = cols.out,
                               overwrite = TRUE ) )
} )

test_that( "Subsetting only columns, by names, no col.names in input", {
  cols.out <- paste0( "cg", subset.seq )
  expect_error(
    envDataSubset( test.read.row.names.in, col.names = cols.out,
                   overwrite = TRUE ) )
} )

# Subsetting rows only

test_that( "Subsetting only rows, by number", {
	rows.out <- subset.seq
	subset.out <- envDataSubset( test.read.in, row.ids = rows.out,
								 overwrite = TRUE )

	expect_s3_class( subset.out, class = "env.data" )
	expect_equal( colnames( subset.out[[ 1 ]] ),
				  paste0( "cg", 1:ncol( test.read.in[[ 1 ]] ) ) )
	expect_equal( rownames( subset.out[[ 1 ]] ),
				  rownames( test.read.in[[ 1 ]] )[ rows.out ] )
} )

test_that( "Subsetting only rows, by names", {
  rows.out <- paste0( "id", subset.seq )
  subset.out <- envDataSubset( test.read.in
	                             , row.names = rows.out
	                             , overwrite = TRUE
	                             , file.out = "subset_out" )
	subset.exported.fileset <- envDataLoad( filename = "subset_out" )

	# checking the output object
	expect_s3_class( subset.out, class = "env.data" )
	expect_equal( colnames( subset.out[[ 1 ]] ),
				  paste0( "cg", 1:ncol( test.read.in[[ 1 ]] ) ) )
	expect_equal( rownames( subset.out[[ 1 ]] ),
				  rownames( test.read.in[[ 1 ]] )[ subset.seq ] )

	# checking the saved object
	expect_s3_class( subset.exported.fileset, class = "env.data" )
	expect_equal( colnames( subset.exported.fileset[[ 1 ]] ),
				  paste0( "cg", 1:ncol( test.read.in[[ 1 ]] ) ) )
	expect_equal( rownames( subset.exported.fileset[[ 1 ]] ),
				  rownames( test.read.in[[ 1 ]] )[ subset.seq ] )
} )

# Subsetting both columns and rows 

test_that( "Subsetting both rows and columns, by names", {
  rows.out <- paste0( "id", subset.seq )
  cols.out <- paste0( "cg", subset.seq )
  subset.out <- envDataSubset( test.read.in
                               , row.names = rows.out
                               , col.names = cols.out
                               , overwrite = TRUE
                               , file.out = "subset_out" )
  subset.exported.fileset <- envDataLoad( filename = "subset_out" )
  
  # checking the output object
  expect_s3_class( subset.out, class = "env.data" )
  expect_equal( colnames( subset.out[[ 1 ]] ),
                cols.out )
  expect_equal( rownames( subset.out[[ 1 ]] ),
                rows.out )
  
  # checking the saved object
  expect_s3_class( subset.exported.fileset, class = "env.data" )
  expect_equal( colnames( subset.exported.fileset[[ 1 ]] ),
                cols.out )
  expect_equal( rownames( subset.exported.fileset[[ 1 ]] ),
                rows.out )
} )

test_that( "Subsetting both rows and columns, by number", {
  rows.out <- subset.seq
  cols.out <- subset.seq
  subset.out <- envDataSubset( test.read.in
                               , row.ids = rows.out
                               , col.ids = cols.out
                               , overwrite = TRUE
                               , file.out = "subset_out" )
  subset.exported.fileset <- envDataLoad( filename = "subset_out" )
  
  # checking the output object
  expect_s3_class( subset.out, class = "env.data" )
  expect_equal( colnames( subset.out[[ 1 ]] ),
                paste0( "cg", cols.out ) )
  expect_equal( rownames( subset.out[[ 1 ]] ),
                paste0( "id", rows.out ) )
  
  # checking the saved object
  expect_s3_class( subset.exported.fileset, class = "env.data" )
  expect_equal( colnames( subset.exported.fileset[[ 1 ]] ),
                paste0( "cg", cols.out ) )
  expect_equal( rownames( subset.exported.fileset[[ 1 ]] ),
                paste0( "id", rows.out ) )
} )

