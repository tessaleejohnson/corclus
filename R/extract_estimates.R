#---------------------------------
# External Dependencies:
# coda
# tibble
# purrr
# rlang
# tidyr
# dplyr
#
# Internal Dependencies:
#
#---------------------------------

#' extract_estimates
#'
#' @param .mod A model object produced by \code{\link[R2MLwiN]{runMLwiN}}.
#'
#' @param .mcmc_nchains Numeric. The number of chains \code{.mod} used.
#' Defaults to 1.
#'
#' @return Returns a tidy table of parameter estimates, including the mean
#' and standard deviation of the saved draws, the 95 percent credible
#' interval, and the effective sample size (as calculated by
#' \code{\link[coda]{effectiveSize}}).
#'
#' @export
#'
#' @examples \dontrun{
#'
#' }
extract_estimates <-
  function(
    .mod,
    .mcmc_nchains = 1
  ) {

    ##--setup--##
    .draws <- .mod@chains

    if (.mcmc_nchains > 1) {
      .draws <- Reduce(rbind, .draws)
    } else {
      .draws <- as.data.frame(.draws)
    }

    # extract data and rebuild as tibble
    .dat <- apply(.draws, 2, as.vector) %>%
      tibble::as_tibble(.)


    ##--extract estimates and pivot to tidy table--##

    .dat %>%
      purrr::imap_dfc(., ~{
        data.frame(
          mean(.x, na.rm = TRUE),
          sd(.x, na.rm = TRUE),
          quantile(.x, 0.025),
          quantile(.x, 0.975),
          coda::effectiveSize(.x)
        ) %>%
          rlang::set_names(
            nm = paste0(
              .y,
              c("#Estimate", "#SE", "#CI_2.5", "#CI_97.5", "#ESS")
            )
          )
      }) %>%
      tidyr::pivot_longer(
        data = .,
        cols = dplyr::everything(),
        names_to = c("Parameter", "Estimate"),
        values_to = "value",
        names_pattern = "(.+)#(.+)"
      ) %>%
      tidyr::pivot_wider(
        data = .,
        names_from = "Estimate",
        values_from = "value"
      )

  }
