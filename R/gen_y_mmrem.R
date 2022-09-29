#---------------------------------
# External Dependencies:
#
# Internal Dependencies:
#
#---------------------------------


#' gen_y_mmrem
#'
#' This function calculates the observed y value from a multiple
#' membership random effects model based on the value of a predictor, x
#' (given in \code{.design_x}), population values of the model coefficients
#' (given in \code{.gamma_x}), school residuals for the schools assigned to
#' person i (given in \code{.sch_resid}), weights for those school residuals
#' (given in \code{.sch_weight}), and person residuals. This population
#' model has fixed slopes and random intercepts. School-level covariates
#' are not included in this model but are used to construct the school residuals
#' in \code{\link{gen_u_mmrem}}.
#'
#' NOTE: function has been changed and documentation requires an update
#'
#' @inheritParams corclus_params
#'
#' @return This function returns a vector of length n (where n is the number
#' of persons in the dataset) of y values calculated based on the population
#' MMREM model. This population model does not explicitly model correlation
#' between school residuals, nor does it account for school-level predictors.
#' Correlations between school residuals and school-level predictors, if
#' desired, must be incorporated into the \code{.sch_resid} values.
#'
#'
#' @examples \dontrun{
#'
#' ## create sample values
#'
#' # gamma vector (intercept, g00 & effect of x on y, g10)
#' g <- c(10, 1.5)
#'
#' # x & design matrix
#' x <- rnorm(5)
#' d_x <- cbind(
#'   xg00 = rep(1, length(x)),
#'   xg10 = x
#' )
#'
#' # school weight matrix
#' s_wt <- cbind(
#'   w1 = c(1, 1, 0.5, 0.5, 0.5),
#'   w2 = c(0, 0, 0.5, 0.5, 0.5)
#' )
#'
#' # school residuals matrix (uncorrelated residuals)
#' s_r <- cbind(
#'   s1 = rnorm(5),
#'   s2 = rnorm(5)
#' )
#'
#' # person residual vector
#' p_r <- rnorm(5)
#'
#' ## generate y values from the population model
#' gen_y_mmrem(
#'   .gamma_x = g,
#'   .x_predictor = d_x,
#'   .sch_weight = s_wt,
#'   .sch_resid = s_r,
#'   .per_resid = p_r
#' )
#'
#' }
gen_y_mmrem <-
  function(
    .dat,
    .x_predictor,
    .sch_weight,
    .sch_resid,
    .per_resid,
    .gamma_x
  ) {

    ##--tidyeval--##

    # tidyeval everything so we can use selector functions or pass
    # the variable names as strings

    .x_pred <- tidyselect::eval_select(
      expr = dplyr::enquo(.x_predictor),
      data = .dat[unique(names(.dat))]
    )

    .sch_wt <- tidyselect::eval_select(
      expr = dplyr::enquo(.sch_weight),
      data = .dat[unique(names(.dat))]
    )

    .sch_res <- tidyselect::eval_select(
      expr = dplyr::enquo(.sch_resid),
      data = .dat[unique(names(.dat))]
    )

    .per_res <- tidyselect::eval_select(
      expr = dplyr::enquo(.per_resid),
      data = .dat[unique(names(.dat))]
    )

    ##--convert objects to matrices--##

    .x_pred_mat <- .dat %>%
      dplyr::select(., !!.x_pred) %>%
      as.matrix(.)

    .sch_wt_mat <- .dat %>%
      dplyr::select(., !!.sch_wt) %>%
      as.matrix(.)

    .sch_res_mat <- .dat %>%
      dplyr::select(., !!.sch_res) %>%
      as.matrix(.)

    .per_res_vec <- .dat %>%
      dplyr::select(., !!.per_res) %>%
      unlist(.) %>%
      as.vector(.)

    ##--pre-combine some elements

    .design_mat <- cbind(1, .x_pred_mat)

    # weight the residuals using element-wise multiplication and
    # sum across each row
    .sch_wres_vec <- rowSums(.sch_wt_mat * .sch_res_mat)

    ##--calculate y--##

    # calculate y
    y <- .design_mat %*% .gamma_x +
        .sch_wres_vec +
        .per_res_vec

    ##--output--##

    y[[1]]

  }
