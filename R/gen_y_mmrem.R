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
#' @param .gamma_x Numeric vector with length p (where p is the number of model
#' coefficients, including the intercept).
#'
#' @param .x_predictor Numeric matrix with dimensions n x p (where n is the num
#' of persons and p is the number of coefficients, including the intercept).
#' The column of \code{.design_x} corresponding to the intercept should be a
#' column of 1s.
#'
#' @param .sch_weight Numeric matrix with dimensions n x h (where n is the
#' number of persons and h is the maximum number of schools attended by any
#' person in the dataset). Rows should sum to 1 (that is, for each student,
#' the weights assigned to their schools attended should sum to 1). For a school
#' a student did not attend, the weight should be 0 (that is, if the maximum
#' number of schools attended was 2 and person A only attended 1 school, then
#' the weight for their "second school" should be 0, while the weight for
#' their "first school" should be 1). To simulate the data, all students were
#' initially assigned a mobility profile (meaning that all students were
#' assigned h schools to attend), and then only a certain proportion of
#' students were coded as mobile. For the students who were coded as mobile,
#' their \code{.sch_weight} matrix row should give equal weight to all schools
#' attended. For students who were coded as non-mobile, their first school
#' was given a weight of 1 and all other schools were given weights of 0.
#'
#' @param .sch_resid Numeric matrix with dimensions n x h (where n is the number
#' of persons and h is the maximum number of schools attended by any person
#' in the dataset). The hth column of the matrix should give the residual for
#' the hth school attended by person i. As mentioned above, all students were
#' initially assigned a mobility profile that included multiple schools, then
#' only a certain proportion of those mobility profiles were retained.
#'
#' @param .per_resid Numeric vector with length n (where n is the number of
#' persons in the data). Gives the person-level residual for the model.
#'
#' @inheritParams pivot_longer_multicol
#'
#' @return This function returns a vector of length n (where n is the number
#' of persons in the dataset) of y values calculated based on the population
#' MMREM model. This population model does not explicitly model correlation
#' between school residuals, nor does it account for school-level predictors.
#' Correlations between school residuals and school-level predictors, if
#' desired, must be incorporated into the \code{.sch_resid} values.
#'
#' @export
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
