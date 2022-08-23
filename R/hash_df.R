#' Hash columns in a data frame
#'
#' The function hash_df accepts a data frame and performs an MD5 has on each column
#' using the openssl package. The original column names and md5 hash checksums are returned as a dataframe.
#'
#' The function may be used in the process of determining which columns have changed in a data frame.
#' @param df The data frame to be hashed.
#' @return A dataframe with two columns: name and hash
#' @export
#' @examples
#' hash_df(iris[1])
#' hash_df(iris)

hash_df <- function(df){
  column_name <- names(df)
  hash <-  as.character(
    openssl::md5(as.character(df))
  )
  data.frame(column_name, hash)
}

