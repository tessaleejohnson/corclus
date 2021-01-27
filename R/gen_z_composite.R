#---------------------------------
# External Dependencies:
#
#
# Internal Dependencies:
#
#---------------------------------


#' gen_z_composite
#'
#' This function takes a matrix of weights \code{.sch_weight} and a matrix
#' of predictor values \code{.sch_predictor} and constructs the weighted
#' sum as \eqn{diag(.sch_weight %*% t(.sch_predictor))}.
#'
#' @param .sch_predictor Numeric matrix or dataframe. Contains the values of
#' the school-level predictor, z, generated in \code{\link{gen_u_mmrem}}.
#'
#' @inheritParams gen_y_mmrem
#'
#' @return This function returns a numeric vector of length equal to
#' \code{NROW(.sch_weight)} (or, equivalently, \code{NROW(.sch_predictor)}).
#' These values are the weighted sum of the predictors constructed for the
#' two schools assigned to all students. For non-mobile students, the
#' z_composite value should be equal to the predictor z from their first
#' school attended.
#'
#' @export
#'
#' @examples \dontrun(
#'
#' sch_inf <-
#'   gen_u_mmrem(
#'     .n_sch = 5,
#'     .clust_cov = c(.8, .1)
#'   ) %>%
#'   expand_sch(., .n_sch = 5, .n_stu = 5) %>%
#'   assign_mobility(., .n_sch = 5) %>%
#'   dplyr::mutate(
#'     .data = .,
#'     z_composite = gen_z_composite(
#'       dplyr::select(., sch_wt_1, sch_wt_2),
#'       dplyr::select(., z_predictor_1, z_predictor_2)
#'     )
#'   )
#'
#' )
gen_z_composite <-
  function(
    .sch_weight,
    .sch_predictor
  ) {

    ##--setup--##

    # check to make sure .sch_predictor and .sch_weight have the same
    # dimensions
    if (!all(dim(.sch_weight) == dim(.sch_predictor))) {
      stop(".sch_weight and .sch_predictor must have the same dimensions.")
    }

    # convert .sch_weight and .sch_predictor to matrices
    wt_mat <- as.matrix(.sch_weight)
    z_mat <- as.matrix(.sch_predictor)


    ##--construct composite with matrix algebra--##

    # matrix multiplying the weights with the transpose of the zs and then
    # taking the diagonal of the result is equivalent (and faster than)
    # multiplying wt_mat[1] * z_mat[1] + wt_mat[2] * z_mat[2]
    z_comp <-
      diag(wt_mat %*% t(z_mat)) %>%
      unname(.)


    ##--output--##
    z_comp

  }
