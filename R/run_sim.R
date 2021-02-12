#---------------------------------
# External Dependencies:
#
# Internal Dependencies:
#
#---------------------------------

#' run_sim
#'
#' @param .sim_fun A function name. The function to be iterated over in the
#' simulations.
#'
#' @param .sim_args A named list of simulation conditions. Each element must
#' match a named argument in \code{.sim_fun}, and each element must have
#' length equal to the number of combinations of conditions. Alternately,
#' pass a call to \code{\link{list_conditions_datagen}} or
#' \code{\link{list_conditions_estimate}}.
#'
#' @param .plan_future A call to \code{\link{plan_future}}.
#'
#' @param .seed_start A logical or numeric. Passed to the \code{seed}
#' argument of \code{\link[furrr]{furrr_options}}. If \code{TRUE}, \code{furrr}
#' will use L'Ecuyer-CMRG RNG streams for parallel-safe random number generation
#' that will produce identical results regardless of the parallel strategy
#' set by \code{.plan_future}. If \code{.seed_start} is a numeric scalar,
#' all runs of the \code{run_sim} function with identical \code{.sim_fun} and
#' \code{.sim_args} will produce identical results.
#'
#' @param ... Parameters passed to \code{sim_fun}.
#'
#' @return A list of objects created by applying \code{.sim_fun} to each
#' combination of conditions given in \code{.sim_args}.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' .n_reps <- 1:2
#'
#' purrr::map(.n_reps, ~run_sim(
#'   .sim_args = list_conditions_datagen(
#'     .n_sch = 5,
#'     .n_stu = 5,
#'     .clust_cov = .8,
#'     .pct_mobile = .5,
#'     .gamma_x = list(c(0, 0))
#'    )
#'  )) %>%
#'  rlang::set_names(., nm = paste0("replication_", .n_reps))
#'
#' }
run_sim <-
  function(
    .sim_fun = generate_data,
    .sim_args = list_conditions_datagen(),
    .plan_future = plan_future(),
    .seed_start = TRUE,
    ...
  ) {

    ##--force evaluation of the .sim_args--##
    force(.sim_args)

    ##--set parallel processing plan--##

    # set plan
    force(.plan_future)

    # reset plan when exiting the function
    on.exit(future::plan(.plan_future), add = TRUE)

    ##--pass .sim_args to .sim_fun--##
    result <-
      .sim_args %>%
      furrr::future_pmap(
        .l = .,
        .f = .sim_fun,
        .options = furrr::furrr_options(
          seed = .seed_start
        )
      )

    ##--output a list of result and the simulation condition--##
    list(result = result, condition = .sim_args)

  }

