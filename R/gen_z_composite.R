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
#' @examples \dontrun{
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
#' }
gen_z_composite <-
  function(
    .dat,
    .sch_weight,
    .sch_predictor
  ) {

    ##--tidyeval--##

    .sch_wt <- tidyselect::eval_select(
      expr = dplyr::enquo(.sch_weight),
      data = .dat[unique(names(.dat))]
    )

    .sch_z <- tidyselect::eval_select(
      expr = dplyr::enquo(.sch_predictor),
      data = .dat[unique(names(.dat))]
    )

    ##--convert objects to matrices--##

    .sch_wt_mat <- .dat %>%
      dplyr::select(., !!.sch_wt) %>%
      as.matrix(.)

    .sch_z_mat <- .dat %>%
      dplyr::select(., !!.sch_z) %>%
      as.matrix(.)


    ##--construct composite with matrix algebra--##

    # weight the residuals using element-wise multiplication and
    # sum across each row
    .sch_wz_vec <- rowSums(.sch_wt_mat * .sch_z_mat)


    ##--output--##
    .sch_wz_vec

  }
