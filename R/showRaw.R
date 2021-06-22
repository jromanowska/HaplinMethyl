#' Show raw environmental data
#' 
#' Extracting the raw measurements from the `env.data` object structure.
#' 
#' @param data Object of class `env.data`.
#' @param rows Vector containing either numbers of rows or row names to show.
#'   By default, if no 'rows' or 'columns' argument is given, the function
#'   returns a 5x5 matrix.
#' @param columns Vector containing either numbers of columns or column names
#'   to show.
#'
#' @return Matrix (in ff format) with extracted data.
#'
#' @export
#' 
showRaw <- function(data = stop("You need to specify the data!", call. = FALSE),
                    rows = 1:5,
                    columns = 1:5
){
  # --- checking validity of arguments ----
	if (!inherits(data, "env.data")){
		stop("Wrong class of the input data set!", call. = FALSE)
	}
  
  if (length(columns) < 1 | length(rows) < 1){
    stop("Number of columns and rows needs to be >= 1!", call. = FALSE)
  }
  
  total_ncol <- ncolumns(data)
  total_nrow <- nrows(data)
  
  data_summary <- summary(data, short = FALSE)
  
  # --- rows -character vector
  if (all(is.character(rows))){
    rows_in_data <- rows %in% data_summary$rownames
    if (any(!rows_in_data)){
      if (sum(rows_in_data) == 0){
        stop("No such row names found in data!", call. = FALSE)
      }
      warning("Not all row names found in the data!")
      rows <- rows[rows_in_data]
    }
  } else if (all(is.numeric(rows))){
  # --- rows -integer vector
    if (max(rows) > total_nrow | min(rows) < 1){
      # extract only the rows that are actually in the dataset
      # but keep the order given by the user!
      sorted_rows <- sort(rows, index.return = TRUE)
      new_rows <- (sorted_rows$x <= total_nrow) & (sorted_rows$x >= 1)
      rows <- rows[sorted_rows$ix[new_rows]]
      if (length(rows) == 0){
        stop("No rows selected!", call. = FALSE)
      }
    }
  } else {
    stop("Problem with 'rows' argument: wrong format!", call. = FALSE)
  }

  # --- columns -character vector
  if (all(is.character(columns))){
    columns_in_data <- columns %in% data_summary$colnames
    if (any(!columns_in_data)){
      if (sum(columns_in_data) == 0){
        stop("No such column names found in data!", call. = FALSE)
      }
      warning("Not all column names found in the data!")
      columns <- columns[columns_in_data]
    }
  } else if (all(is.numeric(columns))){
  # --- columns -integer vector
    if (max(columns) > total_ncol | min(columns) < 1){
      # extract only the columns that are actually in the dataset
      sorted_columns <- sort(columns, index.return = TRUE)
      new_columns <- (sorted_columns$x <= total_ncol) & (sorted_columns$x >= 1)
      columns <- columns[sorted_columns$ix[new_columns]]
      if (length(columns) == 0){
        stop("No columns selected!", call. = FALSE)
      }
    }
  } else {
    stop("Problem with 'columns' argument: wrong format!", call. = FALSE)
  }
  
  # --- extracting ----
  show_matrix <- Haplin:::f.get.gen.data.cols(
    gen.data = data,
    cols = columns,
    by.colname = all(is.character(columns))
  )[rows, ]
  rownames(show_matrix) <- data_summary$rownames[rows]
  return(show_matrix)
}
