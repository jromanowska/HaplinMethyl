# this function is called after loading the library and before sealing the internal package environment
# here, set some variables that will be used by functions within the package
.onLoad <- function( libname, pkgname ){
	if( !requireNamespace( "Haplin" ) ){
		stop( "The 'Haplin' package needs to be installed before 'HaplinMethyl'!" )
	}

	assign( ".haplinMethEnv", new.env(), envir = parent.env( environment() ) )

	# the name for the columns in the ff data
	assign( ".env.cols.name", "env.cols", envir = .haplinMethEnv )

	# setting the class of the read in data
	#   - continuous data:
	assign( ".class.data.env.cont", c( "env.cont", "env.data" ), envir = .haplinMethEnv )
	#   - categorical data:
	assign( ".class.data.env.cat", c( "env.cat", "env.data" ), envir = .haplinMethEnv )

	# set the ff finalizer to delete the files after an ff object is removed
	options( fffinalizer = "delete" )
	options( fffinonexit = TRUE )
}
