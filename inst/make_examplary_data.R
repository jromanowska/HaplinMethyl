# --- create DNAm mock dataset ----
# creating exemplary data: matrix with values [0;1]
nrow.out <- 200
ncol.out <- 400

col.names <- paste0( "cg", seq_len( ncol.out ) )
row.names <- paste0( "id", seq_len( nrow.out ) )

data.tmp <- matrix( runif( ncol.out * nrow.out ), ncol = ncol.out )
colnames( data.tmp ) <- col.names
rownames( data.tmp ) <- row.names

source( "../tests/testthat/common_vars.R" )
# write the data with both column and row names
write.table( rbind( c( 0, col.names ),
				cbind( row.names, data.tmp )
			 ),
			 file = my.env.data.all.names.in,
			 quote = FALSE,
			 row.names = FALSE,
			 col.names = FALSE )

# write the data with only the column names
write.table( rbind( col.names, data.tmp ),
			 file = my.env.data.col.names.in,
			 quote = FALSE,
			 row.names = FALSE,
			 col.names = FALSE )

# write the data with only the row names
write.table( cbind( row.names, data.tmp ),
			 file = my.env.data.row.names.in,
			 quote = FALSE,
			 row.names = FALSE,
			 col.names = FALSE )

# write only the row names
write( row.names, file = only.row.names, ncolumns = length( row.names ) )

# write only the column names
write( col.names, file = only.col.names, ncolumns = length( col.names ) )

# --- create genotype mock dataset ----
library(Haplin)

set.seed( 146 )

hapSim(
  nall = 2,
  n.strata = 3,
  cases = list(c(mfc = 66), c(mfc = 67), c(mfc = 67)),
  # controls = c(mfc = 800),
  haplo.freq = rep(0.25, 2),
  RR = list(c(1.3,1), c(1,1), c(0.6,1)),
  RRstar = c(1,1),
  gen.missing.cases = NULL, gen.missing.controls = NULL,
  n.sim = 1, dire = "strata3", ask = TRUE, verbose = TRUE, cpus = 1
)

data_in <- genDataRead(
  file.in = "strata3/sim1.dat",
  dir.out = "strata3",
  format = "haplin",
  n.vars = 1, allele.sep = " ", col.sep = " "
)
# data_in <- genDataLoad(
#   filename = "sim1", dir.in = "~/Naukowe/Haplin_sim_tests/strata3/"
# )
data_in

showPheno(data_in)
