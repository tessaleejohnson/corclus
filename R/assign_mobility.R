#---------------------------------
# External Dependencies:
# dplyr
#
# Internal Dependencies:
# simulate_mobility
#---------------------------------



#' assign_mobility
#'
#' This function uses mobility assignments simulated by
#' \code{\link{simulate_mobility}} to identify a student's second school
#' information. If the student is not mobile, their second school will be
#' equal to their first school. If they are mobile, their second school ID will
#' be determined by their first school ID plus their mobility indicator (i.e.,
#' representing a change to an adjacent, or nearly adjacent, school). For
#' students attending the first school (i.e., ID = 1) or the last school
#' (i.e., ID = total number of schools), their second school, if they are
#' mobile, will be schools above (for ID = 1) or schools below (for ID = max
#' school ID).
#'
#' Note: this function was designed for a simulation in which the maximum
#' number of schools attended by each student is equal to 2. The code would
#' need to be updated if it is desired that students could attend more than
#' 2 schools.
#'
#' @param .sch_exp A matrix or dataframe. The school-level information created
#' by the \code{\link{gen_u_mmrem}} function and expanded by the
#' \code{\link{expand_sch_info}} function.
#'
#' @param .wt_nonmob Logical. Indicates whether non-mobile students should
#' receive the same weights as mobile students. Technically, it shouldn't
#' matter if non-mobile students are given the same weights because their
#' first and second schools are the same, so all weighting schemes should
#' be equivalent, but it may matter for passing data to MLwiN for estimation.
#' See \code{\link[R2MLwiN]{runMLwiN}} for more information.
#'
#' @param .wt_vec A numeric vector with length equal to the maximum number
#' of schools attended by students in the data (in this simulation, the
#' maximum number is 2). The values in \code{.wt_vec} are used to weight
#' the effects of different schools attended on students. For this study,
#' all mobile students must have the same weights. If different weighting
#' patterns are desired, the code will need to be updated. Defaults to
#' \code{.wt_vec = rep(0.5, 2)}.
#' @param ... Other parameters passed to \code{\link{simulate_mobility}}.
#'
#' @inheritParams gen_z_varcov
#'
#' @return This function returns a dataframe with a number of columns equal to
#' \code{NCOL(.sch_exp) * 2 + 3}. The columns from \code{.sch_exp} are repeated
#' for the second school, and the mobility assignments from
#' \code{\link{simulate_mobility}} along with the first and second school
#' weights are appended as the final 3 columns.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' library(magrittr)
#'
#' sch_inf <-
#'   gen_u_mmrem(
#'     .n_sch = 50,
#'     clust_cov = c(0.8, 0.2, 0.1)
#'   ) %>%
#'   expand_sch(., .n_sch = 50, n_stu = 50) %>%
#'   assign_mobility
#'
#' }
assign_mobility <-
  function(
    .sch_exp,
    .n_sch,
    .wt_nonmob = FALSE,
    .wt_vec = rep(0.5, 2),
    ...
  ) {

    ##--setup--##

    # check if .sch_exp is a dataframe
    if (!is.data.frame(.sch_exp)) {
      stop(".sch_exp must have data.frame class.")
    }

    # create objects to hold the number of rows and columns of .sch_exp
    n_row <- NROW(.sch_exp)
    n_col <- NCOL(.sch_exp)

    # create placeholder list to hold the 2nd school mobility assignment data
    sch2_mob <- as.data.frame(matrix(NA, nrow = n_row, ncol = n_col))
    names(sch2_mob) <- paste0(names(.sch_exp), "_2")

    # append a "_1" tag to names of .sch_exp
    names(.sch_exp) <- paste0(names(.sch_exp), "_1")

    # simulate mobility assignments for all rows in the data
    mob_defauls <- list(draws = n_row, max_dist = 1)
    mob_assign <- simulate_mobility(.draws = n_row, ...)

    # create a weighting scheme for students:
    # option 1 (.wt_nonmob = TRUE): non-mobile students will receive a
    # weight of weight[1] for their first school and weight[2] for their
    # second school
    # option 2 (.wt_nonmob = FALSE): non-mobile students will receive
    # a weight of 1 for their first school and a weight of 0 for their
    # second school (which is just a repeat of the first school)

    # start with option 1:
    # in this case, everyone receives the weights given in .wt_vec
    # (it shouldn't matter for non-mobile students because their
    # first and second schools are the same, but it might matter in
    # how MLwiN works)
    weight_assign <-
      data.frame(
        matrix(.wt_vec, nrow = n_row, ncol = length(.wt_vec))
      )

    names(weight_assign) <- c("sch_wt_1", "sch_wt_2")

    # if .wt_nonmob = FALSE, replace weights for non-mobile students
    # with 1/0s
    if (!.wt_nonmob) {

      # for non-mobile students, replace the weights in weight_assign with
      # a set of 1/0 weights (that is, full weight on school 1 and 0 weight
      # on school 2)

      # create non-mobile weight vector
      weights_nonmob <- c(1, rep(0, length(.wt_vec) - 1))

      wt_dat <-
        apply(
          cbind(weight_assign, mob_assign),
          MARGIN = 1,
          FUN = function(x) {
            if (x[3] == 0) {
              x[1:2] <- weights_nonmob
            }
            x[1:2]
          }
        ) %>%
        t(.) %>%
        as.data.frame(.)

    }

    # return to the unexpanded form of .sch_exp (with no duplicate rows)
    # for use in assigning mobility
    sch_dat <-
      .sch_exp %>%
      dplyr::distinct(.)


    ##--loop--##

    # loop over the rows of .sch_exp to assign second school information
    # for all students (everyone gets a second school, but for some, that
    # school is the same as their first school)

    for (i in 1:n_row) {

      # if the first school attended is school 1 and the ith student is mobile,
      # take the absolute value of their mobility assignment to ensure that
      # the student goes to school 2 next rather than to the non-existent
      # school 0.
      #
      # alternately, if the first school attended is the school with the
      # highest school id (e.g., the fifth school out of 5 schools) and the
      # ith student is mobile, take the absolute value of their mobility
      # assignment and multiply it by -1 to ensure that student goes to the
      # 4th school (out of 5) next rather than the non-existent 6th school.

      # identify the first school attended by the ith student
      first_sch <- .sch_exp$sch_id_1[i]

      if (mob_assign[i] != 0) {
        # check if first school attended is school 1
        if (first_sch == 1) {
          mob_assign[i] <- abs(mob_assign[i])

          # now check if first school attended is school .n_sch (max school)
        } else if (first_sch == .n_sch){
          mob_assign[i] <- -1 * abs(mob_assign[i])
        }
      }

      # now add the first school to the mobility assignment to get the
      # ith student's second school
      new_sch <- first_sch + mob_assign[i]

      # set the ith student's second school info to be equal to that school's
      # residual/predictor data
      sch2_mob[i, ] <- sch_dat[new_sch, ]

    }


    ##--combine the first & second school info--##

    # in addition, add the mobility assignments and school weights
    sch_mob <- cbind(
      .sch_exp,
      sch2_mob,
      mobilty = mob_assign,
      wt_dat
    )


    ##--output--##
    sch_mob


  }
