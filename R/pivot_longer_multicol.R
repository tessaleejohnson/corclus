#---------------------------------
# External Dependencies:
# tidyr
# stringr
#
# Internal Dependencies:
#
#---------------------------------


#' pivot_longer_multicol
#'
#' This function chains two pivots from \code{\link[tidyr]{pivot_longer}} with
#' one pivot from \code{\link[tidyr]{pivot_wider}} to pivot from wide to long
#' on multiple columns. The columns to be pivoted must all have a "tag" in
#' in the original column name (e.g., "sch_1" and "sch_2" have a "_1" and a "_2"
#' tag indicating that data from two schools is present in the dataset). The
#' names must all have some common formatting/symbols before the tag in order
#' to allow all variables to be manipulated at the same time using this
#' function.
#'
#' @param .dat A dataframe containing the data to be pivoted.
#'
#' @param .cols Either a string vector listing the names of the columns to be
#' pivoted or a \code{\link[tidyselect]{starts_with}} selection helper.
#'
#' @param .capture_groups A string containing a regular expression defining
#' two capture groups, one capturing the name of the variable to be saved and
#' one group capturing the commonly formatted "tag" of the variables. Defaults
#' to \code{"(.+)_(.+)"}, which splits the variable names at the last "_"
#' character present in the variable name (the final "_" is removed).
#'
#' @param .tag_group Numeric scalar. Indicates the capture group (as defined
#' by the regular expression in \code{.capture_groups}) that contains the
#' commonly formatted "tag". Defaults to 2 (e.g., "sch_1" contains the tag in
#' the second group, while "t1_sch" contains a tag in the first). The tags will
#' eventually be passed to a variable (named by \code{.tag_name}), so it is
#' helpful if the tags are meaningful. For example, for variables
#' "sch_1" and "sch_2", a new variable will be created that will take values of
#' "1" and "2".
#'
#' @param .tag_name String. Indicates the variable name that should be given to
#' the new variable containing the variable tags. Because this function was
#' created to manage the longitudinal school mobility process, the argument
#' defaults to "time".
#'
#' @param ... Not used at this time.
#'
#' @return This function returns a dataframe with the columns in \code{.cols}
#' pivoted to have a single column per unique value of the non-tag in
#' the capture group and a number of rows equal to the number of unique
#' values in the tag. NOTE: If the variables to be pivoted contain different
#' tag lengths (e.g., "sch_1" and "sch_2" have a tag with length 2 while
#' "id_1", "id_2", "id_3", and "id_4" have a tag with length 4), the variable
#' with the shorter tag length will have missing values where the tags do not
#' overlap.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' # prepare some data
#'
#' temp_dat <- tibble(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   z_1 = rep(c(1, 0), 50),
#'   z_2 = rep(c(5, 6, 7, 8), 25),
#'   g_1 = rnorm(100),
#'   g_2 = rnorm(100),
#'   g_3 = rnorm(100),
#'   g_4 = rnorm(100)
#' )
#'
#' pivot_longer_multicol(
#'   .dat = temp_dat,
#'   .cols = tidyr::matches("_")
#' )
#'
#' }
pivot_longer_multicol <-
  function(
    .dat,
    .cols,
    .capture_groups = "(.+)_(.+)",
    .tag_group = 2,
    .tag_name = "time",
    ...
  ) {

    ##--setup--##

    # Determine the new variable names after applying the regex with
    # .capture_groups to the first pivot_longer step
    # NOTE: we can only have two capture groups for pivot_longer -
    # one to capture the variable name and one to capture the repetition tag
    # (e.g., "sch_id" and "1" or "2"). With two capture groups,
    # stringr::str_match will output a matrix with three columns. The first
    # column will contains the full variable name (since we're matching
    # the whole thing), the second column will contain the first capture
    # group, and the third column will contain the second capture group.
    # The output from the second or third column will contain the new set of
    # columns we want, depending on the regex supplied in .capture_groups
    # and the format of the variable names (i.e., the value of .tag_group).
    col_nm_groups <-
      .dat %>%
      dplyr::select(., .cols) %>%
      names(.) %>%
      stringr::str_match(., .capture_groups)

    value_nms <- unique(col_nm_groups[, .tag_group + 1])

    # use .tag_group (the position of the "tag" or the repeated indicator
    # for the variable names) to set up the input for the "names_to" argument
    # in pivot_longer. with two capture groups, names_to needs to take a
    # vector input of length 2. One of the elements should be the special
    # ".value" sentinel, which uses the values in
    # col_nm_groups[, .tag_group + 1] to set names for the new variables.
    # the other element should be "names" (could be anything, but "names"
    # makes the most sense in our context). This refers to the variable names
    # that we want to save for later (e.g., "sch_id", "sch_wt", "z_predictor").
    # the order of the "names_to" vector input should be set by .tag_group.
    # if the tag is in capture group 2, then ".value" should be in position
    # 2 and "names" should be in position 1. If the tag is in capture group 1,
    # then the order should be swapped accordingly.

    # to accomplish this step, create an empty character vector, assign
    # ".value" to the .tag_group index, and then assign "names" to the
    # remaining index (which can be identified by is.na(namesto_input))

    namesto_input <- character()
    namesto_input[.tag_group] <- ".value"
    namesto_input[is.na(namesto_input)] <- "names"


    ##--convert mobility data from wide to long

    # because there are three groups of wide variables to pivot
    # (sch_id*, z_predictor*, sch_wt*), we need multiple steps:
    #
    # 1. pivot_longer, taking all of the *_1 & *_2 variables
    # and sending their names to one column ("names") and sending their
    # values to two different columns ("1" and "2")
    # 2. pivot longer again, reducing the names of the two values columns
    # ("1" and "2") to one column, given by .tag_name, and the values of
    # those columns to a new column called "value"
    # 3. pivot wider, expanding the variable names in "names" to three different
    # columns, "sch_id", "z_predictor", and "sch_wt". The values for these
    # new variables are pulled from the "values" column. There should now
    # be two rows for each person in the data, and the "values" and
    # "names" columns are dropped.

    .dat %>%
      tidyr::pivot_longer(
        data = .,
        cols = .cols,
        names_to = namesto_input,
        names_pattern = .capture_groups
      ) %>%
      tidyr::pivot_longer(
        data = .,
        cols = value_nms,
        names_to = .tag_name,
        values_to = "values"
      ) %>%
      tidyr::pivot_wider(
        data = .,
        names_from = "names",
        values_from = "values"
      )

  }
