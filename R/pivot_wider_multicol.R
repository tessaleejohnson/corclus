#---------------------------------
# External Dependencies:
# tidyr
# dplyr
# tidyselect
# glue
#
# Internal Dependencies:
# pivot_longer_values
#---------------------------------


#' pivot_wider_multicol
#'
#' This function uses a combination of \code{\link{pivot_longer_multicol}},
#' \code{\link[tidyr]{pivot_wider}}, and \code{\link[dplyr]{group_by}} with
#' \code{\link[dplyr]{summarise}} to expand the weights for each school
#' a student attended to a single column per school ID.
#'
#' @param .id_cols A string or \code{\link[tidyselect]{starts_with}} selection
#' helper that identifies row ID.
#'
#' @param .wider_names A string. Identifies the "tagless" variable that will
#' be used to construct names for the new expanded weight variables. For
#' example, if the original, wide-format variable is "sch_id_1", the "tagless"
#' variable will be "sch_id". Ultimately, this "tagless" name will be
#' determined by the regex given by \code{.capture_groups} passed to
#' \code{\link{pivot_longer_multicol}}.
#'
#' @param .wider_values A string. Identifies the "tagless" variable that will
#' be used to add values to the new expanded weight variables. For
#' example, if the original, wide-format variable is "sch_wt_1", the "tagless"
#' variable will be "sch_wt". Ultimately, this "tagless" name will be
#' determined by the regex given by \code{.capture_groups} passed to
#' \code{\link{pivot_longer_multicol}}.
#'
#' @param .values_fill A scalar. Indicates the value that should be filled in
#' when values are missing. Defaults to 0.
#'
#' @param .collapse_fun A function. Identifies the function to be used to
#' collapse over rows during \code{\link[dplyr]{group_by}} and
#' \code{\link[dplyr]{summarise}}. Defaults to \code{sum}.
#'
#' @param ... Other parameters passed to \code{\link{pivot_longer_multicol}}.
#'
#' @inheritParams pivot_longer_multicol
#'
#' @return This function outputs a dataframe with the same number of rows as
#' the original input, but with only the new columns created by expanding
#' the weight variables plus the ID columns given in \code{.id_cols}.
#' The new dataframe should have a number of columns
#' equal to the unique values of \code{.wider_names}.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' # construct fake data
#' temp_dat <-
#'   tibble::tibble(
#'     x = rnorm(100),
#'     y = rnorm(100),
#'     z_1 = rep(c(1, 0), 50),
#'     z_2 = rep(c(5, 6, 7, 8), 25),
#'     g_1 = rnorm(100),
#'     g_2 = rnorm(100)
#'   )
#'
#' temp_dat %>%
#'   pivot_wider_multicol(
#'     .dat = .,
#'     .id_cols = "x",
#'     .cols = tidyr::matches("_"),
#'     .wider_names = "z",
#'     .wider_values = "g",
#'     .wider_prefix = "new_"
#'   )
#'
#' }
pivot_wider_multicol <-
  function(
    .dat,
    .id_cols,
    .cols,
    .wider_names,
    .wider_values,
    .wider_prefix = "wts_",
    .tag_name = "time",
    .values_fill = 0,
    .collapse_fun = sum,
    ...
  ) {


    ##--pivot--##

    ##--pivot_longer_multicol:

    # use enquo and eval_select to evaluate the .cols function arg
    .cols <-
      tidyselect::eval_select(
        expr = dplyr::enquo(.cols),
        data = .dat[unique(names(.dat))]
     )

    # start with the reduced data and use pivot_longer_multicol to
    # convert it from wide to long
    .dat_long <-
      .dat %>%
      pivot_longer_multicol(
        .dat = .,
        .cols = !!.cols,
        ...
      )


    ##--pivot_wider to expand weights:

    .wider_names_long <-
      tidyselect::eval_select(
        expr = dplyr::enquo(.wider_names),
        data = .dat_long[unique(names(.dat_long))]
      )

    .wider_values_long <-
      tidyselect::eval_select(
        expr = dplyr::enquo(.wider_values),
        data = .dat_long[unique(names(.dat_long))]
      )

    # use pivot_wider to expand the weights such that each school ID
    # receives its own column with values equal to that student's weight
    # for that school (and then dump the .tag_name variable that is
    # a product of this function)
    #
    # note: the names_glue arg uses an actual glue call to implement the
    # {{ }} embrace syntax from dplyr

    .dat_exp <-
      .dat_long %>%
      tidyr::pivot_wider(
        data = .,
        names_from = !!.wider_names_long,
        values_from = !!.wider_values_long,
        names_glue = glue::glue('
          @ .wider_prefix @{ @ names(.dat_long[.wider_names_long]) @ }
          ',
          .open = "@",
          .close = "@"),
        values_fill = .values_fill) %>%
      dplyr::select(., -tidyselect::any_of(.tag_name))


    ##--group_by & summarise to aggregate over duplicated rows

    # use enquo and eval_select to evaluate the .id_cols function arg
    .id_cols_exp <-
      tidyselect::eval_select(
        expr = dplyr::enquo(.id_cols),
        data = .dat_exp[unique(names(.dat_exp))]
      )

    # the above will leave you still with multiple rows per ID.
    # to solve this, we combine rows by calculating the column sums
    # for all rows that have the same ID
    #
    # note: in group_by we use the tidyselect {{ }} syntax
    # to select on the variables given in .id_cols

    .out_dat <-
      .dat_exp %>%
      dplyr::group_by(
        .data = .,
        dplyr::across({{ .id_cols_exp }})
      ) %>%
      dplyr::summarise(
        .data = .,
        dplyr::across(
          .cols = tidyr::matches(.wider_prefix),
          .fns = .collapse_fun,
          na.rm = TRUE,
          .names = "{.col}"
        )
      )

    ##--output--##
    .out_dat


  }
