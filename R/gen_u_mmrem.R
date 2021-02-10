#---------------------------------
# External Dependencies:
# mvnfast
#
# Internal Dependencies:
# gen_z_varcov
#---------------------------------


#' gen_u_mmrem
#'
#' This function generates the residual term for the random slope in the
#' multiple membership random effects model used in this simulation
#' study. To construct a random effect that is correlated across clusters,
#' a predictor, z, is generated with variance-covariance matrix built by
#' \code{\link{gen_z_varcov}}. The cluster-correlated predictor is then used
#' in this function to predict the random slope residual, u0j, with explained
#' variance given by \code{clust_cov} (an argument passed to
#' \code{gen_z_varcov}) and unexplained variance given by \code{.resid_var}.
#' Because no other school-level predictors will be used in this simulation,
#' total variance of the random intercept should be
#' \code{clust_cov[1] + .resid_var}.
#'
#' NOTE: this function relies on the \code{\link[mvnfast]{rmvn}} and
#' \code{\link{gen_z_varcov}}, and \code{\link{is_off_diag}} functions, the
#' first two of which (especially \code{rmvn}) get quite clunky when matrix
#' sizes are large. Make sure that \code{.n_sch} is given a reasonable value to
#' avoid this. In testing, it seems that 5000 works slowly, 10000 is very slow,
#' and any more than that brings things to a complete halt.
#'
#' @inheritParams corclus_params
#'
#' @return This function returns a dataframe with four elements: "sch_id",
#' the ID variable (equal to \code{seq_len(.n_sch)}); "u_residual", the random
#' intercept residual term for each school (equal to predictor_z + v_residual);
#' "z_predictor", the value of the school-level predictor, z, for each school;
#' and "v_residual", the residual term for "u_residual" (i.e., the residual
#' after predicting u_residual from z_predictor) for each
#' school. Each element is vector with length equal to \code{.n_sch}.
#'
#'
#' @examples \dontrun{
#'
#' # set the number of schools
#' j <- 50
#'
#' # set the residual's residual variance
#' v <- 0.2
#'
#' # set the predictor's variance (and covariance with other schools, if
#' desired - see \code{gen_z_varcov} for more info)
#' z <- c(0.8, 0.3, 0.1) # variance (0.8); covar school k/k+1 (0.3); k/k+2 (0.1)
#'
#' # output school-level information
#' gen_u_mmrem(
#'   .n_sch = j,
#'   .u_resid_var = v,
#'   .clust_cov = z
#' )
#'
#' }
gen_u_mmrem <-
  function(
    .n_sch,
    .u_resid_var,
    .clust_cov,
    .gamma_z
  ) {

    ##--setup--##

    # create an ID for the u0j school random intercept residual
    id <- seq_len(.n_sch)


    ##--generate data--##

    # generate multivariate normal values for the school-level predictor_z
    predictor_z <-
      mvnfast::rmvn(
        n = 1,
        mu = rep(0, .n_sch),
        sigma = gen_z_varcov(.n_sch = .n_sch, .clust_cov = .clust_cov)
      ) %>%
      t(.)

    # construct a residual (v_residual) for the school-level random intercept
    # residual, u0j, with variance equal to .resid_var. .resid_var is the
    # residual variance in the random intercept after controlling for
    # predictor_z. if predictor_z has a population variance of 0.8
    # (i.e., .clust_cov = 0.8) and u_residual is equal to 0.2, then
    # u_residual represents (.2/(0.2 + 0.8)), or 20% of
    # the school-level random intercept variance
    v_residual <- rnorm(
      n = .n_sch,
      mean = 0,
      sd = sqrt(.u_resid_var)
    )

    # finally, construct the school-level random intercept residual, u:
    # u0j = predictor_z + u_residual
    u0j <- .gamma_z * predictor_z + v_residual


    ##--output--##
    data.frame(
      sch_id = id,
      u_residual = u0j,
      z_predictor = predictor_z,
      v_residual = v_residual
    )

  }
