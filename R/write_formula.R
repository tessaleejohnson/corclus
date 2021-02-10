#---------------------------------
# External Dependencies:
#
# Internal Dependencies:
#
#---------------------------------

#' write_formula
#'
#' @param response_var String. Gives the name of the response variable. Note:
#' if the response variable is not normally distributed, the link must be
#' included (see \code{\link[R2MLwiN]{runMLwiN}}) for details. Also note: for
#' non-normal links, a constant variable (given as "cons" or "denom" in the
#' \code{R2MLwiN}) documentation must be added to the data and included in the
#' link. For example: \code{"logit(outcome, cons)"}.
#'
#' @param predictor_var A vector of strings. Gives the names of the predictor
#' variables (fixed effects). For an unconditional model, set
#' \code{predictor_var = NULL}.
#'
#' @param level1_id A string. Gives the name of the level-1 id variable.
#'
#' @param level2_id1 A string. Gives the name of the first level-2 id variable.
#'
#' @param level3_id1 A string. Gives the name of the first level-3 id variable.
#'
#' @param random_l1_var A vector of strings. Gives the names of the variables
#' to be used to explain complex level-1 variation.
#'
#' @param random_l2_var A vector of strings. Gives the names of the variables
#' to be used to explain complex level-2 variation.
#'
#' @param random_l3_var A vector of strings. Gives the names of the variables
#' to be used to explain complex level-3 variation.
#'
#' @param intercept String. Indicates the value of the intercept. The intercept
#' defaults to 1. As of 2-10-21, \code{\link[R2MLwiN]{runMLwiN}} fails when
#' a non-1 intercept is given. For mixed distribution or multi-category
#' outcomes, a constant intercept can be specified using brackets:
#' \code{"1[1]"}.
#'
#' @return A formula object.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' write_formula("y", c("x_predictor", "z_composite"))
#'
#' }
write_formula <-
  function(
    response_var = "y",
    predictor_var = NULL,
    level1_id = "stu_id",
    level2_id1 = "sch_id_1",
    level3_id1 = NULL,
    random_l1_var = NULL,
    random_l2_var = NULL,
    random_l3_var = NULL,
    intercept = "1"
  ) {

    ##--setup--##

    # intercept
    if (!is.null(intercept)) {
      intercept_specs <- paste(intercept, " + ")
    } else {
      intercept_specs <- ""
    }

    # predictors
    if (!is.null(predictor_var)) {
      predictor_specs <- paste(paste(predictor_var, collapse = " + "), "+")
    } else {
      predictor_specs <- ""
    }

    # random effects - level 1
    if (!is.null(random_l1_var)) {
      random_l1_specs <- paste("+", paste(random_l1_var, collapse = " + "))
    } else {
      random_l1_specs <- ""
    }

    # random effects - level 2
    if (!is.null(random_l2_var)) {
      random_l2_specs <- paste("+", paste(random_l2_var, collapse = " + "))
    } else {
      random_l2_specs <- ""
    }

    # random effects - level 3
    if (!is.null(random_l3_var)) {
      random_l3_specs <- paste("+", paste(random_l3_var, collapse = " + "))
    } else {
      random_l3_specs <- ""
    }

    if (!is.null(level3_id1)) {
      level3_specs <- glue::glue({'
        + (1 {random_l3_specs}| {level3_id1})
        '})
    } else {
      level3_specs <- ""
    }


    ##--create formula string--##
    formula_string <- glue::glue({'
      {response_var} ~ {intercept_specs}
        {predictor_specs}
        (1 {random_l1_specs}| {level1_id}) +
        (1 {random_l2_specs}| {level2_id1}) {level3_specs}
      '})

    ##--export string as formula--##
    #formula_string
    as.formula(formula_string)
  }
