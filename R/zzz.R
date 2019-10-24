# this function is called after loading the library and before sealing the internal package environment
# here, set some variables that will be used by functions within the package
.onLoad <- function( libname, pkgname ){
	if( !requireNamespace( "Haplin" ) ){
		stop( "The 'Haplin' package needs to be installed before 'HaplinMethyl'!" )
	}

	assign( ".haplinMethEnv", new.env(), envir = parent.env( environment() ) )

	# setting the class of the read in data
	#   - continuous data:
	assign( ".class.data.env.cont", c( "env.cont", "env.data" ), envir = .haplinMethEnv )
	#   - categorical data:
	assign( ".class.data.env.cat", c( "env.cat", "env.data" ), envir = .haplinMethEnv )
}
