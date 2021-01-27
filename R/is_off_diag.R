#---------------------------------
# External Dependencies:
# glue
#
# Internal Dependencies:
#
#---------------------------------


#' is_off_diag
#'
#' This function tests an input matrix and returns a logical matrix of the
#' same dimensions, with \code{TRUE} indicating that a cell is in the desired
#' off-diagonal. By default, the function returns the sub-diagonal (i.e.,
#' the lower off-diagonal).
#'
#' @param .dat A matrix or dataframe.
#'
#' @param .off_diag A numeric scalar. \code{NROW(.dat) - .off_diag} must
#' be greater than or equal to 1. If a negative value is entered, the absolute
#' value will be taken. If \code{.off_diag = 0}, the primary matrix diagonal is
#' identified. Defaults to 1.
#'
#' @param sub_diag Logical. Indicates whether the sub-diagonal (lower
#' off-diagonal) or the super-diagonal (upper off-diagonal) should be
#' identified. If \code{sub_diag = TRUE}, the sub-diagonal will be identified,
#' if \code{sub_diag = FALSE}, the super-diagonal will be returned instead.
#' Defaults to \code{TRUE}.
#'
#' @return This function returns a logical matrix with the same dimensions as
#' \code{.dat}. All cells but those lying on the desired off-diagonal will take
#' the value of \code{FALSE}, while those on the off-diagonal will return
#' a value of \code{TRUE}.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' nr <- 4
#' nc <- 6
#'
#' mat <- matrix(seq(4*6), nrow = nr, ncol = nc)
#' df <- as.data.frame(mat)
#'
#' # works for matrices
#' is_off_diag(mat, .off_diag = 2)
#' is_off_diag(mat, .off_diag = 2, sub_diag = F)
#'
#' # and dataframes
#' is_off_diag(df, .off_diag = 2)
#' is_off_diag(df, .off_diag = 2, sub_diag = F)
#'
#' # check for equality
#' all(is_off_diag(mat, 2) == is_off_diag(df, 2))
#' all(is_off_diag(mat, 2, sub_diag = F) == is_off_diag(df, 2, sub_diag = F))
#'
#' }
is_off_diag <-
  function(
    .dat,
    .off_diag = 1,
    sub_diag = TRUE
  ) {

    ##--setup--##

    # if .off_diag is negative, take the absolute value
    .off_diag <- abs(.off_diag)

    # identify the number of rows and columns
    n_row <- NROW(.dat)
    n_col <- NCOL(.dat)

    # create an empty logical matrix filled with "FALSE"
    lgl_dat <- matrix(FALSE, nrow = n_row, ncol = n_col)


    ##--loop--##

    # if sub_diag is TRUE, loop over rows; if FALSE, loop over columns

    if (sub_diag) {

      # throw an error if the number of rows minus the requested .off_diag is
      # negative
      if ((n_row - .off_diag) < 1) {
        stop(
          glue::glue('
          .off_diag must be less than or equal to {n_row - 1}.
          ')
        )
      }

      # loop over rows of lgl_dat & mark .off_diag cells as TRUE
      for (i in 1:(n_row - .off_diag)) {
        if (i <= n_col) {
          lgl_dat[i + .off_diag, i] <- TRUE
        }
      }

    } else {

      # throw an error if the number of cols minus the requested .off_diag is
      # negative
      if ((n_col - .off_diag) < 1) {
        stop(
          glue::glue('
          .off_diag must be less than or equal to {n_col - 1}.
          ')
        )
      }

      # loop over cols of lgl_dat & mark .off_diag cells as TRUE
      for (i in 1:(n_col - .off_diag)) {
        if (i <= n_row) {
          lgl_dat[i, i + .off_diag] <- TRUE
        }
      }
    }


    ##--output--##
    lgl_dat

  }



