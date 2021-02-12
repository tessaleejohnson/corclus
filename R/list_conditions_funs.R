#---------------------------------
# External Dependencies:
# purrr
# rlang
#
# Internal Dependencies:
#
#---------------------------------


#' list_conditions
#'
#' This function belongs in the interior of
#' \code{\link{list_conditions_datagen}} and
#' \code{\link{list_conditions_estimate}}. It takes the arguments passed to
#' its parent function and returns a list of all combinations of those
#' arguments.
#'
#' The argument combinations are created using \code{\link[purrr]{cross}} to
#' create a list where each element is a combination (list length is equal
#' to the number of combinations), and then the list is transformed using
#' \code{\link[purrr]{transpose}} to flip it "inside out" such that the
#' result has length equal to the number of arguments passed to the parent
#' function, and each argument has length equal to the number of combinations.
#'
#' @return A named list of all combinations of arguments given in the parent
#' function. The list has length equal to the number of arguments passed to
#' the function and each argument has length equal to the number of
#' combinations.
#'
list_conditions <-
  function() {

    ##--grab all arguments of the caller function--##

    # pulls the formals, or defaults
    default_args <- rlang::fn_fmls(fn = rlang::caller_fn(n = 1L))

    # pulls the newly defined args
    new_args <- rlang::call_args(sys.call(sys.parent()))

    # overwrite the duplicated default_args with the new_args
    # note: purrr::list_modify was attempted first, but it relies on
    # list_recurse, which doesn't seem to handle problems quickly (it just
    # recurses for a long time before spitting an error)
    update_nms <- names(new_args)[names(new_args) %in% names(default_args)]

    all_args <- default_args
    all_args[update_nms] <- new_args

    # evaluate each element in the list because rlang::fm_fmls &
    # rlang::call_args return lists of unevaluated arguments
    arg_list <- purrr::map(all_args, eval)


    ##--expand all conditions and transpose as purrr::pmap-ready list--##

    # purrr::cross exports a list with each element as a combination;
    # purrr::transpose then turns the list "inside out" so that each element
    # is a named argument with length equal to the number of combinations

    # first, name the new condition list
    all_combos <-
      purrr::cross(arg_list) %>%
      rlang::set_names(., nm = paste0("condition_", seq_along(.)))

    # then, flip the list inside out
    all_flipped <- all_combos %>%
      purrr::transpose(.)


    ##--output--##
    all_flipped

  }



#' list_conditions_datagen
#'
#' Wraps around \code{\link{list_conditions}} to expand all combinations of
#' the arguments passed to the function. The arguments can
#' then be passed to \code{purrr::pmap} (\code{\link[purrr]{map2}}) to iterate
#' over all simulation conditions.
#'
#' @details NOTE: All arguments that take objects with length > 1 must
#' wrap those objects in a list.
#' Bad: \code{list_conditions_datagen(.clust_cov = c(0.8, 0))}.
#' Good: \code{list_conditions_datagen(.clust_cov = list(c(0.8, 0)))}.
#'
#' @inheritParams generate_data
#'
#' @return This function returns a list of all combinations of arguments
#' passed to this function. Each list element is an argument with length
#' equal to the number of combinations.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' list_conditions_datagen(.nsch = 5, .pct_mobile = 0.1)
#'
#' }
list_conditions_datagen <-
  function(
    .n_sch = 50,
    .n_stu = 50,
    .u_resid_var = 0.2,
    .clust_cov = list(
      c(0.8, 0),
      c(0.8, 0.2),
      c(0.8, 0.4)
    ),
    .wt_vec = list(c(0.5, 0.5)),
    .pct_mobile = c(0.0, 0.25, 0.5),
    .mean_x = 5,
    .var_x = 4,
    .mean_r = 0,
    .var_r = 2,
    .gamma_z = 1,
    .gamma_x = list(
      c(10, sqrt(17)/2),
      c(10, sqrt(11/3)/2),
      c(10, 1/(2*sqrt(3)))
    )
  ) {

    ##--expand all conditions and export as list--##
    list_conditions()

  }


#' list_conditions_estimate
#'
#' This function wraps around \code{\link{list_conditions}} to expand all
#' combinations of the arguments passed to the function. The arguments can
#' then be passed to \code{purrr::pmap} (\code{\link[purrr]{map2}}) to iterate
#' over all simulation conditions.
#'
#' @details NOTE: All arguments that take objects with length > 1 must
#' wrap those objects in a list.
#' Bad: \code{list_conditions_datagen(.clust_cov = c(0.8, 0))}.
#' Good: \code{list_conditions_datagen(.clust_cov = list(c(0.8, 0)))}.
#'
#' Bad: \code{list_conditions_estimate(.dat = generate_data())}.
#' Good: \code{list_conditions_estimate(.dat = list(generate_data()))}.
#'
#' The same applies to the \code{.model_formula} and \code{.mm_list} args,
#' even when only "one" condition is present. (That is, a single formula
#' with both a LHS and a RHS has length 3: "~", "LHS" and "RHS"; similarly,
#' for one multiple membership condition, \code{\link{build_mm_list}}
#' produces a list with length 2).
#'
#' @inheritParams run_mlwin
#'
#' @export
#'
#' @return This function returns a list of all combinations of arguments
#' passed to this function. Each list element is an argument with length
#' equal to the number of combinations.
#'
#' @examples \dontrun{
#'
#' list_conditions_estimate()
#'
#' }
list_conditions_estimate <-
  function(
    .dat,
    .model_formula = lapply(
      list(
        NULL,
        "x_predictor",
        "z_composite",
        c("x_predictor", "z_composite")
      ),
      function(v) write_formula(predictor_var = v)
    ),
    .outcome_dist = "Normal",
    .mcmc_burn = 500,
    .mcmc_iter = 5000,
    .mcmc_nchains = 1,
    .mm_list = list(HLM = NA, MMREM = build_mm_list(.n_clust = 2))
  ) {

    ##--expand all conditions and export as list--##
    list_conditions()


  }




