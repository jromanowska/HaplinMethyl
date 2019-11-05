context( "Testing envDataSubset function." )

source( "common_vars.R" )

test.read.in <- envDataLoad( my.env.data.all.names.out )
subset.seq <- c( 3, 5, 10, 121 )

test_that( "Subsetting only columns, by number", {
	cols.out <- subset.seq
	subset.out <- envDataSubset( test.read.in, col.ids = cols.out, overwrite = TRUE )

	expect_s3_class( subset.out, class = "env.data" )
	expect_equal( colnames( subset.out[[ 1 ]] ),
				  paste0( "cg", cols.out ) )
	expect_equal( rownames( subset.out[[ 1 ]] ),
				  rownames( test.read.in[[ 1 ]] ) )
} )

test_that( "Subsetting only rows, by number", {
	rows.out <- subset.seq
	subset.out <- envDataSubset( test.read.in, row.ids = rows.out, overwrite = TRUE )

	expect_s3_class( subset.out, class = "env.data" )
	expect_equal( colnames( subset.out[[ 1 ]] ),
				  paste0( "cg", 1:ncol( test.read.in[[ 1 ]] ) ) )
	expect_equal( rownames( subset.out[[ 1 ]] ),
				  rownames( test.read.in[[ 1 ]] )[ rows.out ] )
} )

test_that( "Subsetting only columns, by names", {
	cols.out <- paste0( "cg", subset.seq )
	subset.out <- envDataSubset( test.read.in, col.names = cols.out, overwrite = TRUE )

	expect_s3_class( subset.out, class = "env.data" )
	expect_equal( colnames( subset.out[[ 1 ]] ),
				  paste0( "cg", subset.seq ) )
	expect_equal( rownames( subset.out[[ 1 ]] ),
				  rownames( test.read.in[[ 1 ]] ) )
} )

test_that( "Subsetting only rows, by number", {
	rows.out <- paste0( "id", subset.seq )
	subset.out <- envDataSubset( test.read.in, row.names = rows.out, overwrite = TRUE )

	expect_s3_class( subset.out, class = "env.data" )
	expect_equal( colnames( subset.out[[ 1 ]] ),
				  paste0( "cg", 1:ncol( test.read.in[[ 1 ]] ) ) )
	expect_equal( rownames( subset.out[[ 1 ]] ),
				  rownames( test.read.in[[ 1 ]] )[ subset.seq ] )
} )
