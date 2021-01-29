#---------------------------------
# External Dependencies:
#
# Internal Dependencies:
# calc_exp_l2var
#---------------------------------



#' calc_exp_icc
#'
#' @inheritParams generate_data
#'
#' @return A numeric scalar.
#' @export
#'
#' @examples \dontrun{
#'
#' }
calc_exp_icc <-
  function(
    .pct_mobile = 0.2,
    .clust_cov = c(0.8, 0.1),
    .wt_vec = c(0.5, 0.5),
    .u_resid_var = 0.2,
    .gamma_z = 1,
    .gamma_x = c(10, 1.5),
    .var_x = 4,
    .var_r = 2
  ) {

    # calculate expected level 2 variance
    l2_var <- calc_exp_l2var(
      .pct_mobile = .pct_mobile,
      .clust_cov = .clust_cov,
      .wt_vec = .wt_vec,
      .u_resid_var = .u_resid_var,
      .gamma_z = .gamma_z
    )

    # calculate total variance
    tot_var <- sum(c(l2_var, (.gamma_x[-1]^2) %*% .var_x, .var_r))

    # and l1_var
    l1_var <- tot_var - l2_var

    ##--output expected values--##
    list(
      l2_var = l2_var,
      l1_var = l1_var,
      tot_var = tot_var
    )


  }
