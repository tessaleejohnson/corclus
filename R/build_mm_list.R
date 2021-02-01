#---------------------------------
# External Dependencies:
#
# Internal Dependencies:
#---------------------------------


#' build_mm_list
#'
#' @param .mm_ids A character vector. The names of the multiple membership
#' unique ID variables.
#'
#' @param .mm_wts A character vector. The names of the multiple membership
#' weight variables.
#'
#' @return This function returns a list to be passed to \code{run_mlwin}.
#'
#' @examples \dontrun{
#'
#' }
build_mm_list <-
  function(
    .mm_ids = paste0("ids_", seq_len(.n_sch)),
    .mm_wts = paste0("wts_", seq_len(.n_sch))
  ) {

    ##--list multiple membership ids--##
    id_list <- as.list(.mm_ids)
    wt_list <- as.list(.mm_wts)


    ##--output list of lists--##
    # note: the NA in the last position represents the student ID
    list(list(mmvar = id_list, weights = wt_list), NA)

  }
