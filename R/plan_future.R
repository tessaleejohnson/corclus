#---------------------------------
# External Dependencies:
# future
#
# Internal Dependencies:
#
#---------------------------------


#' plan_future
#'
#' This function is a wrapper for \code{\link[future]{plan}} to set the
#' parallel processing plans for running simulations in R.
#'
#' @param .plan A call to \code{\link[future]{multisession}} or
#' \code{\link[future]{sequential}}.
#'
#' @param .n_cores Numeric scalar or call to
#' \code{\link[future]{availableCores}}.
#'
#' @param ... Other parameters passed to the
#' \code{\link[future]{sequential}} or \code{\link[future]{multisession}}
#' strategies identified in \code{.plan}.
#'
#' @return Sets the parallel processing plan for resolving futures. See
#' \code{\link[future]{plan}} for output.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' plan_future("multisession", 3)
#'
#' }
plan_future <-
  function(
    .plan = c("sequential", "multisession"),
    .n_cores = future::availableCores() - 1,
    ...
  ) {

    ##--setup--##

    .plan <- match.arg(.plan, choices = c("sequential", "multisession"))

    ## setup cores for future_pmap
    if (.plan == "sequential") {
      future::plan(strategy = future::sequential, ...)
    } else if (.plan == "multisession") {
      future::plan(strategy = future::multisession, workers = .n_cores, ...)
    }

  }
