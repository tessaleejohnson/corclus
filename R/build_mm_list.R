#---------------------------------
# External Dependencies:
#
# Internal Dependencies:
#---------------------------------


#' build_mm_list
#'
#' @param .n_clusters Numeric scalar. The number of clusters in the data.
#'
#' @param .mm_id_nms A string. The prefix name of the multiple membership
#' unique ID variables.
#'
#' @param .mm_wt_nms A string. The prefix name of the multiple membership
#' weight variables.
#'
#' @return This function returns a list to be passed to \code{run_mlwin}.
#'
#' @examples \dontrun{
#'
#' }
build_mm_list <-
  function(
    .n_clusters,
    .mm_id_nms = "ids",
    .mm_wt_nms = "wts"
  ) {

    ##--setup--##
    .mm_ids <- paste0(.mm_id_nms, seq_len(.n_clusters))
    .mm_wts <- paste0(.mm_wt_nms, seq_len(.n_clusters))

    ##--list multiple membership ids--##
    id_list <- as.list(.mm_ids)
    wt_list <- as.list(.mm_wts)


    ##--output list of lists--##
    # note: the NA in the last position represents the student ID
    list(list(mmvar = id_list, weights = wt_list), NA)

  }
