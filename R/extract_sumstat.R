#---------------------------------
# External Dependencies:
# tibble
#
# Internal Dependencies:
#
#---------------------------------

#' extract_sumstat
#'
#' @inheritParams extract_estimates
#'
#' @return This function returns a single-column dataframe with model fit
#' estimates (DIC).
#'
#' @export
#'
#' @examples \dontrun{
#'
#'
#' }
extract_sumstat <-
  function(
    .mod
  ) {

    # extract DIC info and coerce to data.frame
    .mod[["BDIC"]] %>%
      data.frame(Model_Fit = .) %>%
      tibble::as_tibble(.)

  }
