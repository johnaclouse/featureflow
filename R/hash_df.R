#' Hash rows or columns in a data frame
#'
#' The function hash_df accepts a data frame and performs an MD5 along the specified margin using the openssl package.
#' When margin = "rows", the original data frame is return with an additional column labeled md5 containing row-level hash values.
#' When margin = "columns", the column names from the original data frame and md5 hash are returned as a data frame.
#'
#' The function may be used in the process of determining which columns have changed in a data frame.
#' @param df The data frame to be hashed.
#' @param margin A character value indicating the margin on which the hash will be calculated (rows or columns).
#' @return A dataframe with two columns: name and hash
#' @export
#' @examples
#' hash_df(iris[1])
#' hash_df(iris)

hash_df <- function(df,
                    margin = c("rows", "columns")){
  if (match.arg(margin) == "rows") {
    df %>%
      mutate(across(everything(), as.character)) %>%
      rowwise %>%
      mutate(md5 =
               digest::digest(c_across(everything()),
                              algo = "md5"))
  } else {
    column_name <- names(df)
    md5 <-  as.character(
      openssl::md5(as.character(df))
    )
    data.frame(column_name, md5)
  }
}

