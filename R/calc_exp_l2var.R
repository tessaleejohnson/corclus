#---------------------------------
# External Dependencies:
#
# Internal Dependencies:
#
#---------------------------------


#' calc_exp_l2var
#'
#' @inheritParams generate_data
#'
#' @return A numeric scalar.
#' @export
#'
#' @examples \dontrun{
#'
#' }
calc_exp_l2var <-
  function(
    .pct_mobile = 0,
    .clust_cov = c(0.8, 1),
    .wt_vec = c(0.5, 0.5),
    .u_resid_var = 0.2,
    .gamma_z = 1
  ) {

    # weight the variances
    w_var <- c()

    for (i in 1:2) {
      w_var[i] <- ((.gamma_z * .wt_vec[i])^2) *
        (.u_resid_var + .clust_cov[1])
    }

    w_var <- sum(w_var)

    # weight the covariance
    w_cov <- c()

    for (i in 1:2) {

      if (i == 1) {
        # not mobile
        .mob_cov <- 1
      } else {
        # mobile
        .mob_cov <- .clust_cov[2]
      }

      w_cov[i] <- 2 * .mob_cov *
        (.gamma_z * .wt_vec[1]) *
        (.gamma_z * .wt_vec[2])

    }

    # weight the sum of w_cov based on proportion of mobility
    m <- .pct_mobile

    w_cov <- (1 - m) * w_cov[1] + m * w_cov[2]

    # output
    w_var + w_cov

  }
