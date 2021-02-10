#---------------------------------
# External Dependencies:
# dplyr
# tibble
# tidyr
# tidyselect
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
    ...
  ) {

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
        .wt_nonmob = FALSE,
        .pct_mobile = .pct_mobile,
        ...
      )

    ##--pivot school info wider

    # 1. add a student identifier, stu_id
    # 2. create a 1/0 coded is_mobile var
    # 3. add expanded sch_wts (wts), sch_ids (ids), & sch_res (ures)


    # helper functions for pivoting wide & then summarizing in the
    # pivot_wider_multicol steps
    combine_vals <-
      function(...) {
        sum(..., na.rm = TRUE)
      }

    collapse_vals <-
      function(...) {
        x_unique <- unique(...)
        x_narm <- x_unique[x_unique != 0]

        if (length(x_narm) == 0) {
          combine_vals(x_narm)
        } else {
          x_narm
        }
      }

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
      ) %>%
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
          .collapse_fun = ~sum(.x, na.rm = TRUE)
        ),
        # expand ids, one column per id
        pivot_wider_multicol(
          .dat = .,
          tidyr::matches("stu"),
          tidyr::matches("sch_id"),
          .wider_names = tidyr::matches("sch_id"),
          .wider_values = tidyr::matches("sch_id"),
          .wider_prefix = "ids_",
          .values_fill = 0,
          .collapse_fun = collapse_vals
        )
      )


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
    # 3. organize variable order by type
    # 4. add a constant term

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

