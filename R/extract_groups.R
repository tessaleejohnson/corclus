#---------------------------------
# External Dependencies:
# tidyr
# dplyr
#
# Internal Dependencies:
# pivot_longer_multicol
#---------------------------------


#' extract_groups
#'
#' This function takes a string input and applies the
#' \code{\link[stringr]{str_match}} function to extract the unique values
#' of the capture group requested by \code{.tag_group} based on a regex
#' given by \code{.capture_groups}.
#'
#' @param .strings A character vector. Gives the strings to be extracted by
#' \code{.capture_groups}.
#'
#' @inheritParams pivot_longer_multicol
#'
#' @return This function returns a character vector that is the result of
#' matching the input \code{.strings} against the regex \code{.capture_groups},
#' selecting the desired capture group, \code{.tag_groups}, and returning
#' the unique values of the result.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' char_vec <- c("extract", "a_b_c_d", "1_m.b-q_m")
#'
#' # accept 1 or more matches in each capture group
#' extract_groups(char_vec, "(.+)_(.+)", 1)
#' extract_groups(char_vec, "(.+)_(.+)", 2)
#'
#' # accept 0 or 1 matches in each capture group
#' extract_groups(char_vec, "(.?)a(.?)", 1)
#' extract_groups(char_vec, "(.?)a(.?)", 2)
#'
#'
#' }
extract_groups <-
  function(
    .strings,
    .capture_groups = "(.+)_(.+)",
    .tag_group = 2
  ) {

    ##--setup--##
    if (.tag_group > 2) {
      stop(
        glue::glue('
          .tag_group must not exceed {str_count(.capture_groups, "\\\\(")}
          (the number of capture groups in "{.capture_groups}").
          ')
      )
    }

    ##--match strings--##

    # determine the new variable names from the
    match_matrix <-
      .strings %>%
      stringr::str_match(., .capture_groups)

    ##--output--##

    # output the resulting, extracted string
    unique(match_matrix[, .tag_group + 1])

  }
