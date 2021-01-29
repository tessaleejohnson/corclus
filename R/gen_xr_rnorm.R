#---------------------------------
# External Dependencies:
#
#
# Internal Dependencies:
#
#---------------------------------


#' gen_xr_rnorm
#'
#' @param .mean_x Numeric scalar. The mean of the predictor, x. Defaults to 5.
#'
#' @param .var_x Numeric scalar. The variance of the predictor, x. Defaults to
#' 4.
#'
#' @param .mean_r Numeric scalar. The mean of the person-level residual, r.
#' Defaults to 0.
#'
#' @param .var_r Numeric scalar. The variance of the person-level residual, r.
#' Defaults to 2.
#'
#' @inheritParams expand_sch
#' @inheritParams simulate_mobility
#'
#' @return This function returns a dataframe with two columns,
#' \code{x_predictor} and \code{r_residual}. The predictor, x, is a person-level
#' predictor of the person-level outcome, y (generated using
#' \code{\link{gen_y_mmrem}}). The person-specific error, or residual, r, and
#' the person-level predictor, x, are both continuous and normally distributed,
#' generated using the \code{\link[stats]{Normal}} function.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' gen_xr_rnorm(.n_stu = 5, .n_sch = 5)
#'
#' }
gen_xr_rnorm <-
  function(
    .n_stu,
    .n_sch,
    .mean_x,
    .var_x,
    .mean_r,
    .var_r,
    .seedling = NULL
  ) {

    ##--setup--##

    # set seed for random number generation
    set.seed(.seedling)

    # define fixed values
    n_obs <- .n_stu * .n_sch


    ##--create person-level data--##

    # person-level predictor
    x <-  rnorm(n_obs, .mean_x, sqrt(.var_x))

    # person-specific residual
    r <-  rnorm(n_obs, .mean_r, sqrt(.var_r))


    ##--output--##
    data.frame(
      x_predictor = x,
      r_residual = r
    )


  }
