#---------------------------------
# External Dependencies:
#
#
# Internal Dependencies:
#
#---------------------------------


#' gen_xr_rnorm
#'
#' @inheritParams corclus_params
#'
#' @return This function returns a dataframe with two columns,
#' \code{x_predictor} and \code{r_residual}. The predictor, x, is a person-level
#' predictor of the person-level outcome, y (generated using
#' \code{\link{gen_y_mmrem}}). The person-specific error, or residual, r, and
#' the person-level predictor, x, are both continuous and normally distributed,
#' generated using the \code{\link[stats]{Normal}} function.
#'
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
    .var_r
  ) {

    ##--setup--##

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
