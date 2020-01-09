context( "Testing envDataRead function." )

source( "common_vars.R" )

test_that( "Reading the data correctly", {
	examples.dir <- system.file( "extdata", package = "HaplinMethyl" )
	test.read.in <- envDataRead( my.env.data.all.names.in,
		dir.in = examples.dir,
		file.out = my.env.data.all.names.out,
		sep = " ",
		cont = TRUE,
		header = TRUE,
		rownames = TRUE,
		overwrite = TRUE )
	info.test <- summary( test.read.in )
	ncols.in <- info.test$ncol

	expect_s3_class( test.read.in, class = "env.data" )
	expect_equal( colnames( test.read.in[[ 1 ]] ),
				  paste0( "cg", seq_len( ncols.in ) ) )
} )

test_that( "Providing vector of row names", {
	examples.dir <- system.file( "extdata", package = "HaplinMethyl" )
	rownames <- scan( file = file.path( examples.dir, only.row.names ) )
	test.read.in <- envDataRead( my.env.data.col.names.in,
		dir.in = examples.dir,
		file.out = my.env.data.col.names.out,
		sep = " ",
		cont = TRUE,
		header = TRUE,
		rownames = rownames,
		overwrite = TRUE )
	info.test <- summary( test.read.in )
	ncols.in <- info.test$ncol

	expect_s3_class( test.read.in, class = "env.data" )
	expect_equal( colnames( test.read.in[[ 1 ]] ),
				  paste0( "cg", seq_len( ncols.in ) ) )
	expect_equal( rownames( test.read.in[[ 1 ]] ),
				  rownames )
} )

test_that( "Providing wrong separator", {
	examples.dir <- system.file( "extdata", package = "HaplinMethyl" )
	expect_error( envDataRead( my.env.data.all.names.in,
		dir.in = examples.dir,
		file.out = my.env.data.all.names.out,
		sep = ":",
		cont = TRUE,
		header = TRUE,
		rownames = TRUE,
		overwrite = TRUE ) )
} )
