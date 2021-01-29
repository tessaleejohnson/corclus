#---------------------------------
# External Dependencies:
# glue
# matrixcalc
#
# Internal Dependencies:
# is_off_diag
#---------------------------------


#' gen_z_varcov
#'
#' This function constructs the variance-covariance matrix used in generating
#' multivariate normal values for a school-level predictor, z. The variance-
#' covariance matrix for the predictor is a square matrix with dimensions of
#' \code{.n_sch} (total number of schools) \code{x .n_sch}. The values given in
#' \code{.clust_cov} define a form of a Toeplitz structure, such that the values
#' on the main diagonal take a value of \code{.clust_cov[1]}, the values on the
#' first off diagonal take a value of \code{.clust_cov[2]}, etc. All values
#' not defined by \code{.clust_cov} default to 0.
#'
#' It is important to note that because this simulation is intended for use
#' with a single school-level predictor, the proportion of variance explained
#' by the predictor out of the total school-level variance will be equal to
#' \code{.clust_cov[1] / (residual variance + .clust_cov[1])}.
#'
#' @param .n_sch Numeric scalar. Gives the total number of schools in the
#' dataset. The variance-covariance matrix for predictor z will have dimensions
#' \code{.n_sch x .n_sch}.
#'
#' @param .clust_cov Numeric vector. The first element of the vector gives the
#' variance of all schools' predictors, z. If present, the second element gives
#' the covariance of z between schools k and k + 1. The values given in
#' \code{.clust_cov} apply to all schools (that is, similar to a Toeplitz
#' pattern). Any off-diagonal values (i.e., covariances) not specified will
#' default to 0. The main diagonal (i.e., predictor variance) defaults to 0.8.
#'
#' @return This function returns a matrix of dimensions \code{.n_sch x .n_sch},
#' the variance-covariance matrix for a school-level predictor, z. This matrix
#' can be used to generate multivariate normal values for predictor z.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' # give values for the variance (0.8), covariance between schools
#' # k & k + 1 (0.3), and schools k and k + 2 (0.1)
#' cov_vec <- c(0.8, 0.3, 0.1)
#'
#' # output the variance-covariance matrix
#' gen_z_varcov(.n_sch = 50, .clust_cov = cov_vec)
#'
#' }
gen_z_varcov <-
  function(
    .n_sch,
    .clust_cov
  ) {

    ##--setup--##

    # check if the length of .clust_cov is less than or equal to .n_sch
    if (length(.clust_cov) > .n_sch) {
      stop(glue::glue('
          length(.clust_cov) must be less than or equal to {.n_sch}.
          It is currently {length(.clust_cov)}.
          ')
      )
    }

    # create a square matrix of dimensions .n_sch x .n_sch, filled with 0s
    z_sigma <- matrix(0, nrow = .n_sch, ncol = .n_sch)


    ##--loops--##

    # first, loop over the length of .clust_cov and use is_off_diag
    # to assign the .clust_cov value associated with the respective
    # off diagonal (for example, the first value of .clust_cov should
    # give the percent variance explained by the school-level predictor, z,
    # and, as such, should be placed on the main diagonal; the second value
    # gives the covariance between the predictors of schools k and k + 1, and
    # should be placed on the first off-diagonal; the third value is the
    # covariance of schools k and k+2 and should be placed on the second
    # off-diagonal; etc...)

    for (i in 1:length(.clust_cov)) {
      z_sigma[is_off_diag(z_sigma, (i - 1))] <- .clust_cov[i]
    }

    # next, loop over all rows and columns of z_sigma to assign the
    # lower-triangular values given in the loop above to the upper triangle
    for (i in 1:nrow(z_sigma)) {
      for(j in 1:i) {
        z_sigma[j, i] <- z_sigma[i, j]
      }
    }

    # test if matrix is positive definite
    if (!matrixcalc::is.positive.definite(z_sigma)) {
      stop("z_sigma is not positive definite. Check the values of .clust_cov.")
    }

    ##--output--##
    z_sigma

  }

