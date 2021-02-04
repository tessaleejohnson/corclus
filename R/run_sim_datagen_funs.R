#---------------------------------
# External Dependencies:
# purrr
# tibble
# tidyr
# dplyr
# rlang
# progress
# furrr
# future
# progressr
# tictoc
# glue
#
# Internal Dependencies:
# generate_data
#---------------------------------


#' run_sim_datagen
#'
#' This function is a wrapper around \code{\link{generate_data}}. Each of
#' the arguments (except \code{.n_reps}) take the values for the conditions.
#' Fixed conditions with length 1 can be input as scalar values. Variable
#' conditions and fixed conditions with length > 1 must be entered as lists
#' with each list element representing the values of the argument for a single
#' condition. All arguments must have length 1 or the length of the input with
#' the max value. For simulations that have different numbers of levels
#' within factors, use \code{\link[base]{rep}} to repeat the values to the
#' appropriate size. For example: \code{.pct_mobile = rep(list(0, .25, .5), 2)}.
#'
#' @param .n_reps Numeric. Either the total number of replications to run or
#' a vector indicating the replications to run. Defaults to 1.
#'
#' @param .n_cores Numeric scalar or call to
#' \code{\link[future]{availableCores}}.
#'
#' @param .plan A call to \code{\link[future]{multisession}} or
#' \code{\link[future]{sequential}}l
#'
#' @inheritParams generate_data
#'
#' @return This function returns a list of length equal to \code{.n_reps}. The
#' second level of the list has length equal to the product of the number of
#' levels of each argument.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' run_sim_datagen(
#'   .n_reps = 1,
#'   .n_sch = 50,
#'   .n_stu = 50,
#'   .u_resid_var = 0.2,
#'   .clust_cov = rep(list(
#'     c(0.8, 0),
#'     c(0.8, 0.25)
#'   ), 3),
#'   .wt_vec = list(c(0.5, 0.5)),
#'   .pct_mobile = rep(list(0, 0.25, 0.5), 2),
#'   .mean_x = 5,
#'   .var_x = 4,
#'   .mean_r = 0,
#'   .var_r = 2,
#'   .gamma_z = 1,
#'   .gamma_x = rep(list(
#'     c(10, sqrt(17)/2),
#'     c(10, sqrt(11/3)/2),
#'     c(10, 1/(2*sqrt(3)))
#'   ), 2)
#' )
#'
#'
#' }
run_sim_datagen <-
  function(
    .n_reps = 1,
    .seed_start = 33,
    .n_cores = future::availableCores() - 1,
    .plan = c("sequential", "multisession"),
    .n_sch = 50,
    .n_stu = 50,
    .u_resid_var = 0.2,
    .clust_cov = list(
      c(0.8, 0),
      c(0.8, 0.25),
      c(0.8, 0.4)
    ),
    .wt_vec = list(c(0.5, 0.5)),
    .pct_mobile = list(0.0, 0.25, 0.5),
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

    ##--setup--##

    .plan <- match.arg(.plan, choices = c("sequential", "multisession"))

    ## setup cores for future_pmap
    if (.plan == "sequential") {
      future::plan(future::sequential)
    } else if (.plan == future::multisession) {
      futre::plan(future::multisession, workers = .n_cores)
    }


    ## set up .n_reps to be a vector
    if (length(.n_reps) == 1) {
      .n_reps <- seq_len(.n_reps)
    }

    ## set a seed
    #.seed_start <- set.seed(.seed_start)


    ##--expand simulation conditions--##

    # 1. grab all the function arguments
    # 2. remove the .n_reps arg using purrr::list_modify and rlang::zap (we'll
    # deal with .n_reps later)
    # 3. conver to tibble and create all possible combinations of the values
    # using tidyr::complete
    # 4. add an argument to index the length of the expanded tibble
    # 5. convert back to a list

    # grab function args
    fun_args <- list(
      .n_reps = .n_reps,
      .n_sch = .n_sch,
      .n_stu = .n_stu,
      .u_resid_var = .u_resid_var,
      .clust_cov = .clust_cov,
      .wt_vec = .wt_vec,
      .pct_mobile = .pct_mobile,
      .mean_x = .mean_x,
      .var_x = .var_x,
      .mean_r = .mean_r,
      .var_r = .var_r,
      .gamma_z = .gamma_z,
      .gamma_x = .gamma_x
    )

    # expand all combinations of the condition levels
    sim_args_expand <- fun_args %>%
      purrr::list_modify(., .n_reps = rlang::zap()) %>%
      tibble::as_tibble(.) %>%
      tidyr::complete(., !!! rlang::syms(names(.))) %>%
      dplyr::mutate(., .cond_index = 1:NROW(.))

    # get the number of simulation conditions
    .n_cond <- NROW(sim_args_expand)

    # convert back to list
    .sim_args <- sim_args_expand %>%
      as.list(.)


    ##--set up progress bar--##

    # set progress bar & start timer
    .prog_reps <- progressr::progressor(steps = max(.n_reps))
    .start_time <- tictoc::tic()

    ##--loop over data gen conditions--##


    # 1. inner loop (run_sim_datagen_inner): use furrr::future_pmap to loop
    # over all the different simulation conditions
    # 2: outer loop (run_sim_datagen_outer): use furrr::future_map

    sim_list <-
      # outer loop (over replications)
      .n_reps %>%
      furrr::future_map(
        .x = .,
        .f = run_sim_datagen_outer,
        .sim_args = .sim_args,
        .n_reps = .n_reps,
        .n_cond = .n_cond,
        .prog_reps = .prog_reps,
        .start_time = .start_time,
        .options = furrr::furrr_options(seed = .seed_start)
      ) %>%
      rlang::set_names(., nm = paste0("rep", .n_reps))


    ##--print the elapsed time
    tictoc::toc()


    ##--output sim results--##
    sim_list

  }


#' run_sim_datagen_outer
#'
#' This is the outermost loop called from \code{\link{run_sim_datagen}} that
#' loops over the simulation condition replications.
#'
#' @param .rep_index Numeric. A unique number identifying the current
#' data gen replication number. Created from within
#' \code{\link{run_sim_datagen}}.
#'
#' @param .sim_args A named list of condition values, created in
#' \code{\link{run_sim_datagen}}.
#'
#' @param .prog_reps An object of class "progress_bar". Created by the
#' \code{\link[progress]{progress_bar}} function in
#' \code{\link{run_sim_datagen}}.
#'
#' @param .n_reps Numeric scalar. The total number of replications to run.
#' Defaults to 1.
#'
#' @param .n_cond Numeric. The total number of unique conditions for data
#' generation. Created in \code{\link{run_sim_datagen}}.
#'
#' @param .start_time Numeric. The elapsed time since the simulation function
#' started (given by \code{\link[tictoc]{tic}}).
#'
#' @param ... Other values from the arguments to \code{\link[purrr]{map}} that
#' are passed to the outer loop of \code{\link{run_sim_datagen}} but not
#' used in the outer loop function.
#'
#' @return This function returns the data generated over all conditions and
#' replications as well as the meta data (e.g., condition information and
#' replication numbers).
#'
run_sim_datagen_outer <-
  function(
    .rep_index,
    .sim_args,
    .n_reps,
    .n_cond,
    .prog_reps,
    .start_time,
    ...
  ){

    ##--setup--##

    ## set progress bar for the conditions (inner) loop
    #.prog_condition <- progressr::progressor(steps = .n_cond)

    # update progress bar for the replications (outer) loop
    .prog_reps(
      glue::glue({'
        Current Rep: {.rep_index} of {max(.n_reps)}
        Completed: {100 * .rep_index/max(.n_reps)}%

        '}),
      class = "sticky")


    ## next set a random seed for the outer loop
    .seed_start <- round(runif(1) * 1e6)


    ##--update list of args to pass to furrr::future_pmap

    # outer loop args to pass to inner loop
    .outer_args <- .sim_args %>%
      purrr::list_modify(
        .x = .,
        .rep_index = .rep_index
      )

    # inner loop (over conditions)
    .outer_args %>%
      furrr::future_pmap(
        .l = .,
        .f = run_sim_datagen_inner,
        .options = furrr::furrr_options(seed = .seed_start)
      ) %>%
      rlang::set_names(., nm = paste0("condition", 1:.n_cond))

  }



#' run_sim_datagen_inner
#'
#' This is the innermost loop called from \code{\link{run_sim_datagen}} that
#' loops over the data generation conditions.
#'
#' @param .rep_index Numeric. A unique number identifying the current
#' data gen replication number. Created from within
#' \code{\link{run_sim_datagen}}.
#'
#' @param ... Other values from the arguments to \code{\link[purrr]{map}} that
#' are passed to the inner loop of \code{\link{run_sim_datagen}} but not
#' used in the inner loop function.
#'
#' @inheritParams generate_data
#'
#' @return This function returns a list of the current condition specifications
#' and the data generated from it.
#'
run_sim_datagen_inner <-
  function(
    .n_sch,
    .n_stu,
    .u_resid_var,
    .clust_cov,
    .wt_vec,
    .pct_mobile,
    .mean_x,
    .var_x,
    .mean_r,
    .var_r,
    .gamma_z,
    .gamma_x,
    .cond_index,
    .rep_index,
    ...
  ){

    ##--start by setting the random seed for the inner loop--##

    .seed_start <- round(runif(1) * 1e6)

    ##--create a list of the args to be fed to generate_data & run--##
    .updated_args <- list(
      .n_sch = .n_sch,
      .n_stu = .n_stu,
      .u_resid_var = .u_resid_var,
      .clust_cov = .clust_cov,
      .wt_vec = .wt_vec,
      .pct_mobile = .pct_mobile,
      .mean_x = .mean_x,
      .var_x = .var_x,
      .mean_r = .mean_r,
      .var_r = .var_r,
      .gamma_z = .gamma_z,
      .gamma_x = .gamma_x,
      .seed_start = .seed_start
    )

    .dat <- rlang::exec(generate_data, !!! .updated_args)

    # create a list of the current sim condition
    .cond_vals <- list(
      .rep_index = .rep_index,
      .cond_index = .cond_index,
      .updated_args
    )


    ##--output the data and condition--##
    list(
      .dat = .dat,
      .cond_vals = .cond_vals
    )



  }
