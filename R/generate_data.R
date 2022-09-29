#---------------------------------
# External Dependencies:
# dplyr
# tibble
# tidyr
# tidyselect
# progressr
#
# Internal Dependencies:
# gen_u_mmrem
# expand_sch
# assign_mobility
# gen_z_composite
# gen_xr_rnorm
# gen_y_mmrem
# pivot_wider_multicol
#---------------------------------


#' generate_data
#'
#' This function generates data according to the multiple membership
#' random effects model with correlated clusters given by the
#' variance-covariance matrix specified in \code{\link{gen_z_varcov}}.
#'
#' @param ... Other parameters passed to \code{\link{assign_mobility}}.
#'
#' @param .mm_format String. Options are "compact" or "wide".
#' Compact formats require two variables, one for the organization ID and
#' one for the weights; the number of variables in each type is equal to
#' the maximum number of organizational memberships for any individual in
#' the dataset. The wide format requires only one variable type, which
#' provides the weight for each person for each organization. MLwiN
#' documentation provides conflicting information about which type is required
#' by \code{\link[R2MLwiN]{runMLwiN}}. In the
#' [Stata documentation](https://www.bristol.ac.uk/media-library/sites/cmm/migrated/documents/13-stata-example.pdf).
#' it is said that both R and Stata require wide format. In the documentation
#' for the \code{R2MLwiN} [package](https://www.jstatsoft.org/article/view/v072i10/v72i10.pdf)
#' multiple membership analysis is demonstrated with compact form. Here, we
#' use compact form as provided in the \code{R2MLwiN} package publication in
#' the Journal of Statistical Software.
#'
#' @param .progress_bar Internal argument passed from \code{\link{run_sim}}.
#' If \code{.progress_bar = TRUE} in the external \code{\link{run_sim}}
#' function, then a progress bar will be displayed while executing this code.
#' The arguments in this internal function defaults to \code{NULL}.
#'
#' @inheritParams corclus_params
#'
#' @return This function returns data generated under the correlated cluster
#' multiple membership model, with correlated clusters defined by the
#' correlation between the school-level predictor z. The result is a tibble
#' with number of rows equal to \code{.n_sch * .n_stu} and a number of columns
#' equal to \code{18 + .n_sch}.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' # with the following defaults, we adjust x1beta to get the
#' # desired icc:
#'
#' ## icc = 0.05
#' # x1beta <- sqrt(17)/2
#'
#' ## icc = 0.15
#' # x1beta <- sqrt(11/3)/2
#'
#' ## icc = 0.30
#' x1beta <- 1/(2*sqrt(3))
#'
#' generate_data(
#'   .n_sch = 5,
#'   .n_stu = 5,
#'   .u_resid_var = 0.2,
#'   .clust_cov = c(.8, 1),
#'   .wt_vec = c(0.5, 0.5),
#'   .pct_mobile = 0,
#'   .mean_x = 5,
#'   .var_x = 4,
#'   .mean_r = 0,
#'   .var_r = 2,
#'   .gamma_z = 0,
#'   .gamma_x = c(10, x1beta)
#' )
#'
#' }
generate_data <-
  function(
    .n_sch = 5,
    .n_stu = 5,
    .u_resid_var = 0.2,
    .clust_cov = 0.8,
    .wt_vec = c(0.5, 0.5),
    .pct_mobile = 0,
    .mean_x = 5,
    .var_x = 4,
    .mean_r = 0,
    .var_r = 2,
    .gamma_z = 0,
    .gamma_x = c(10, sqrt(17)/2),
    .mm_format = c("compact", "wide"),
    .progress_bar = NULL,
    ...
  ) {

    ##--setup--##

    # start progress bar
    if (!is.null(.progress_bar)) {
      .progress_bar()
    }

    # match .mm_format argument
    .mm_format <- match.arg(.mm_format, choices = c("compact", "wide"))


    ##--generate school information:

    sch_inf <-
      gen_u_mmrem(
        .n_sch = .n_sch,
        .u_resid_var = .u_resid_var,
        .clust_cov = .clust_cov,
        .gamma_z = .gamma_z
      ) %>%
      expand_sch(., .n_sch = .n_sch, .n_stu = .n_stu) %>%
      assign_mobility(
        .sch_exp = .,
        .n_sch = .n_sch,
        .wt_vec = .wt_vec,
        .pct_mobile = .pct_mobile,
        ...
      )

    ##--pivot school info wider

    # 1. add a student identifier, stu_id
    # 2. create a 1/0 coded is_mobile var
    # 3. if .mm_format is "wide", add expanded sch_wts (wts) to the dataset

    # expand the school information
    sch_wider <-
      sch_inf %>%
      tibble::as_tibble(.) %>%
      tibble::rowid_to_column(., var = "stu_id") %>%
      dplyr::mutate(
        .data = .,
        is_mobile = dplyr::case_when(
          mobility == 0 ~ 0,
          is.na(mobility) ~ NA_real_,
          TRUE ~ 1
        )
      )


    # if the multiple membership format is selected as "wide", create
    # one weight variable per multiple membership unit filled with the
    # unit's weight for each student
    if (.mm_format == "wide") {

      sch_wider <-
        sch_wider %>%
        dplyr::mutate(
          .data = .,
          # expand weights, one column per id
          pivot_wider_multicol(
            .dat = .,
            tidyr::matches("stu"),
            tidyr::matches("sch"),
            .wider_names = tidyr::matches("sch_id"),
            .wider_values = tidyr::matches("sch_wt"),
            .wider_prefix = "wts_",
            .values_fill = 0,
            .aggregator_fun = combine_vals
          )
        )

    }


    ##--generate student information:

    # 1. add student level predictor (x) and residual (y)
    # 2. add student level outcome (y)
    # 3. add school level composite predictor (z_composite)

    sch_stu_dat <-
      sch_wider %>%

      # randomly simulate one person-level predictor and residual
      dplyr::mutate(
        .data = .,
        gen_xr_rnorm(
          .n_stu = .n_stu,
          .n_sch = .n_sch,
          .mean_x = .mean_x,
          .var_x = .var_x,
          .mean_r = .mean_r,
          .var_r = .var_r
        )
      ) %>%

      # generate y based on the population mmrem model (random intercept,
      # fixed slope)
      dplyr::mutate(
        y = gen_y_mmrem(
          .dat = .,
          .gamma_x =  .gamma_x,
          .sch_weight = tidyr::matches("sch_wt"),
          .sch_resid = tidyr::matches("u_res"),
          .per_resid = "r_residual",
          .x_predictor = "x_predictor"
        )
      ) %>%

      # add composite z predictor by weighting the original zs and
      # computing the rowSums
      dplyr::mutate(
        z_composite = gen_z_composite(
          .dat = .,
          .sch_weight = tidyr::matches("sch_wt"),
          .sch_predictor = tidyr::matches("z_pred")
        )
      )


    ##--data manipulation:

    # 1. order data by sch1's id (sch_id_1) and stu_id
    # 2. organize variable order by type
    # 3. add a constant term

    dat_sorted <-
      sch_stu_dat %>%
      dplyr::arrange(., sch_id_1, stu_id) %>%
      dplyr::select(
        .data = .,
        tidyr::matches("_id"),
        tidyr::matches("sch_wt"),
        tidyr::matches("mob"),
        tidyselect::all_of(c(
          "y",
          "x_predictor",
          "z_composite",
          "z_predictor_1",
          "z_predictor_2"
        )),
        tidyr::matches("residual"),
        tidyr::matches("ids_"),
        tidyr::matches("wts_")
      )

    ##--output data--##
    dat_sorted


  }

