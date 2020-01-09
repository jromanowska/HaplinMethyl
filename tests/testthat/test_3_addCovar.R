context( "Testing addCovar function." )

source( "common_vars.R" )

dir.exmpl <- system.file( "extdata", package = "Haplin" )
exemplary.file1 <- paste0( dir.exmpl, "/HAPLIN.trialdata.txt" )

my.gen.data.haplin <- Haplin::genDataRead( file.in = exemplary.file1, file.out = "trial_data1",
	dir.out = tempdir(), format = "haplin", n.vars = 0, overwrite = TRUE )

exemplary.file2 <- paste0( dir.exmpl, "/HAPLIN.trialdata2.txt" )
my.gen.data.haplin2 <- Haplin::genDataRead( file.in = exemplary.file2, file.out = "trial_data2",
	dir.out = tempdir(), format = "haplin", n.vars = 2, allele.sep = "", overwrite = TRUE )

exemplary.file3 <- paste0( dir.exmpl, "/exmpl_data.ped" )
my.gen.data <- Haplin::genDataRead( exemplary.file3, file.out = "ped_data",
	dir.out = tempdir(), format = "ped", overwrite = TRUE )

test_that( "Adding covar to haplin-formatted data without covariates", {
	covar.vect <- sample( x = 1:3, size = nrow( my.gen.data.haplin$gen.data[[ 1 ]]),
						  replace = TRUE )
	c.name <- "add.c"
	new.data <- addCovar( my.gen.data.haplin, covar = covar.vect, c.name = c.name )
	expect_s3_class( new.data, class( my.gen.data.haplin ) )
	expect_true( is.matrix( new.data$cov.data ) )
	expect_equal( ncol( new.data$cov.data ), 1 )
	expect_identical( colnames( new.data$cov.data ), c.name )
} )

test_that( "Adding covar to haplin-formatted data with covariates", {
	covar.vect <- sample( x = 1:3, size = nrow( my.gen.data.haplin2$gen.data[[ 1 ]]),
						  replace = TRUE )
	c.name <- "add.c"
	new.data <- addCovar( my.gen.data.haplin2, covar = covar.vect, c.name = c.name )
	expect_s3_class( new.data, class( my.gen.data.haplin2 ) )
	expect_true( is.matrix( new.data$cov.data ) )
	expect_equal( ncol( new.data$cov.data ),
					  ncol( my.gen.data.haplin2$cov.data ) + 1 )
	expect_identical( colnames( new.data$cov.data ),
					  c( colnames( my.gen.data.haplin2$cov.data ), c.name ) )
} )

test_that( "Adding covar to ped-formatted data", {
	covar.vect <- sample( x = 1:3, size = nrow( my.gen.data$gen.data[[ 1 ]]),
						  replace = TRUE )
	c.name <- "add.c"
	new.data <- addCovar( my.gen.data, covar = covar.vect, c.name = c.name )
	expect_s3_class( new.data, class( my.gen.data ) )
	expect_true( is.matrix( new.data$cov.data ) )
	expect_equal( ncol( new.data$cov.data ),
					  ncol( my.gen.data$cov.data ) + 1 )
	expect_identical( colnames( new.data$cov.data ),
					  c( colnames( my.gen.data$cov.data ), c.name ) )
} )

test_that( "Wrong length of covar vector", {
	covar.vect <- sample( x = 1:3, size = nrow( my.gen.data.haplin$gen.data[[ 1 ]]) - 3,
						  replace = TRUE )
	c.name <- "add.c"
	expect_error( addCovar( my.gen.data.haplin, covar = covar.vect, c.name = c.name ) )
} )

