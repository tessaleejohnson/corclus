#---------------------------------
# External Dependencies:
#
# Internal Dependencies:
#---------------------------------


#' build_mm_list
#'
#' @param .n_clust Numeric scalar. The number of clusters.
#' Depending on whether the "compact" (default) or "wide" format was used in
#' to \code{\link{generate_data}}, this argument either refers to the total
#' number of clusters in the dataset ("wide"), or the maximum number of clusters
#' an individual can be a member of ("compact"). For example, if students
#' attended a maximum of 2 schools, but there were 50 schools in the dataset,
#' for the "wide" option, set \code{.n_clust = 50}; for the "compact"
#' option, set \code{.n_clust = 2}.
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
    .n_clust,
    .mm_id_nms = "sch_id_",
    .mm_wt_nms = "sch_wt_"
  ) {

    ##--setup--##
    .mm_ids <- paste0(.mm_id_nms, seq_len(.n_clust))
    .mm_wts <- paste0(.mm_wt_nms, seq_len(.n_clust))

    ##--list multiple membership ids--##
    id_list <- as.list(.mm_ids)
    wt_list <- as.list(.mm_wts)


    ##--output list of lists--##
    # note: the NA in the last position represents the student ID
    list(list(mmvar = id_list, weights = wt_list), NA)

  }
