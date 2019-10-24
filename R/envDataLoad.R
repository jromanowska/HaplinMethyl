#' Loading the data previously read in and saved by "envDataRead"
#'
#' This function loads the data from the saved .ffData and .RData files,
#' and prepares the data to subsequent analysis.
#'
#' @param filename The base of the filenames; i.e. if the data is saved in
#'  "my_data_env.ffData" and "my_data_env.RData", then the 'filename'
#'  should be "my_data".
#' @param dir.in The path to the directory where files were saved (defaults to the
#'  current directory).
#'
#' @return A list of ffdf objects with the environmental data in numeric format.
#'
#' @export
#'
envDataLoad <- function( filename = stop( "'filename' must be given!" ),
						 dir.in = "." )
{
	file.in.ff <- paste( dir.in, "/", filename, "_env.ffData", sep = "" )
	if( !file.exists( file.in.ff ) ){
		stop( "The file(s) doesn't seem to exist!", call. = FALSE )
	}

	env.cols.name <- get( ".env.cols.name", envir = Haplin:::.haplinEnv )
	file.in.base <- paste( dir.in, "/", filename, "_env", sep = "" )
	suppressWarnings( ff::ffload( file.in.base, rootpath = getOption( "fftempdir" ) ) )
	loaded.objects <- ls( pattern = paste0( env.cols.name, ".[[:digit:]]" ) )

	# maintain the correct order!
	if( length( loaded.objects ) > 9 ){
		chunks.no <- vapply( loaded.objects, FUN = function(x){
			tmp.string <- unlist( strsplit( x, split = ".", fixed = TRUE ) )
			return( as.numeric( tmp.string[ length( tmp.string ) ] ) )
		},
		FUN.VALUE = seq_along( loaded.objects ),
		USE.NAMES = FALSE )
		sorted.chunks <- sort( chunks.no, index.return = TRUE )
		loaded.objects <- loaded.objects[ sorted.chunks$ix ]
	}

	env.data.col.wise <- list()
	for( i in loaded.objects ){
		cur.chunk <-  get( i )
		if( !is.null( dim( cur.chunk ) ) ){
			env.data.col.wise <- c( env.data.col.wise, list( cur.chunk ) )
		}
		rm( cur.chunk )
	}

	return( env.data.col.wise )
}
