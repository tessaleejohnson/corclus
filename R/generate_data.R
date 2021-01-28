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
#' This function
#'
#' @param .seed_start Numeric scalar. The starting seed value. Each function
#' in this script that uses random number generation will take this starting
#' seed and add some noise to it. Defaults to 1.
#'
#' @param ... Other parameters passed to \code{\link{assign_mobility}}.
#'
#' @inheritParams gen_z_varcov
#' @inheritParams expand_sch
#' @inheritParams gen_y_mmrem
#' @inheritParams simulate_mobility
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
#' generate_data()
#'
#' }
generate_data <-
  function(
    .n_sch = 5,
    .n_stu = 5,
    .clust_cov = c(.8, .2),
    .pct_mobile = 0.2,
    .gamma = c(10, 1.5),
    .seed_start = 1,
    ...
  ) {


    ##--generate school information:

    sch_inf <-
      gen_u_mmrem(
        .n_sch = .n_sch,
        .clust_cov = .clust_cov,
        .seedling = .seed_start + 1
      ) %>%
      expand_sch(., .n_sch = .n_sch, .n_stu = .n_stu) %>%
      assign_mobility(
        .sch_exp = .,
        .n_sch = .n_sch,
        .pct_mobile = .pct_mobile,
        .seedling = .seed_start + 2,
        ...
      ) %>%
      dplyr::mutate(
        .data = .,
        z_composite = gen_z_composite(
          dplyr::select(., sch_wt_1, sch_wt_2),
          dplyr::select(., z_predictor_1, z_predictor_2)
        )
      )

    ##--generate student information:

    # create student ID as a sequence from 1 to the number of students per
    # school multiplied by the total number of schools (essentially just the
    # row numbers)
    stu_id <- seq_len(.n_sch * .n_stu)

    # randomly simulate one person-level predictor and residual
    xr <- gen_xr_rnorm(
      .n_stu = .n_stu,
      .n_sch = .n_sch,
      .seedling = .seed_start + 3
    )

    # generate y based on the population mmrem model (random intercept,
    # fixed slope)
    y <- gen_y_mmrem(
      .gamma =  .gamma,
      .sch_weight = cbind(sch_inf$sch_wt_1, sch_inf$sch_wt_2),
      .sch_resid = cbind(sch_inf$u_residual_1, sch_inf$u_residual_2),
      .per_resid = xr$r_residual,
      .design_x = cbind(1, xr$x_predictor)
    )

    # combine person-level data
    stu_inf <- cbind(stu_id, xr, y)


    ##--data manipulation:

    # 1. combine school and student datasets,
    # 2. add expanded sch_wts & sch_ids to the data,
    # 3. create a 1/0 coded is_mobile var,
    # 4. order results by sch1's id (wts_1) and stu_id
    # 5. organize variable order by type
    # 6. add a constant term

    #sch_stu_dat <-
      tibble::tibble(sch_inf, stu_inf) %>%
      dplyr::mutate(
        .data = .,
        is_mobile = dplyr::case_when(
          mobility == 0 ~ 0,
          is.na(mobility) ~ NA_real_,
          TRUE ~ 1
        ),
        constant = 1
      ) %>%
      dplyr::mutate(
        .data = .,
        pivot_wider_multicol(
          .dat = .,
          tidyr::matches("stu"),
          tidyr::matches("sch"),
          .wider_names = tidyr::matches("sch_id"),
          .wider_values = tidyr::matches("sch_wt"),
          .wider_prefix = "wts_",
          .values_fill = 0,
          .collapse_fun = sum
        ),
        pivot_wider_multicol(
          .dat = .,
          tidyr::matches("stu"),
          tidyr::matches("sch_id"),
          .wider_names = tidyr::matches("sch_id"),
          .wider_values = tidyr::matches("sch_id"),
          .wider_prefix = "ids_",
          .values_fill = 0,
          .collapse_fun = max
        )
      ) %>%
      dplyr::arrange(., sch_id_1, sch_id_2, stu_id) %>%
      dplyr::select(
        .data = .,
        tidyr::matches("_id"),
        tidyr::matches("mob"),
        tidyselect::all_of(c(
          "constant",
          "y",
          "x_predictor",
          "z_composite",
          "z_predictor_1",
          "z_predictor_2"
        )),
        tidyr::matches("sch_wt"),
        tidyr::matches("wts_"),
        tidyr::matches("ids_"),
        tidyr::matches("residual")
      )

    ##--output data--##
    sch_stu_dat


  }


