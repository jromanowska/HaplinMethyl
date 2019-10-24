# creating exemplary data: matrix with values [0;1]
nrow.out <- 200
ncol.out <- 1000

col.names <- paste0( "cg", seq_len( ncol.out ) )
row.names <- paste0( "id", seq_len( nrow.out ) )

data.tmp <- matrix( runif( ncol.out * nrow.out ), ncol = ncol.out )
colnames( data.tmp ) <- col.names
rownames( data.tmp ) <- row.names

source( "../tests/common_vars.R" )
# write the data with both column and row names
write.table( data.tmp,
			 file = my.env.data.all.names.in,
			 quote = FALSE,
			 row.names = TRUE,
			 col.names = TRUE )

# write the data with only the column names
write.table( data.tmp,
			 file = my.env.data.col.names.in,
			 quote = FALSE,
			 row.names = FALSE,
			 col.names = TRUE )

# write the data with only the row names
write.table( data.tmp,
			 file = my.env.data.row.names.in,
			 quote = FALSE,
			 row.names = TRUE,
			 col.names = FALSE )
