context( "Testing envDataRead function." )

source( "common_vars.R" )

test_that( "Reading the data correctly", {
	test.read.in <- envDataRead( my.env.data.all.names.in,
		dir.in = file.path( "..", "..", "inst", "extdata" ),
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
