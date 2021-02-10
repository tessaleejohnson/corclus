#---------------------------------
# External Dependencies:
#
# Internal Dependencies:
#
#---------------------------------


#' simulate_mobility
#'
#' @param .draws Numeric scalar. The number of draws from the distribution,
#' likely the number of persons in the data.
#'
#' @param .max_dist Numeric scalar. The general number of schools away from a
#' student's home school that they should be allowed to move. Defaults to 1.
#'
#' @param .p_success Numeric scalar between 0 and 1. The probability of a
#' success. The way this is coded, success translates to mobility, so lower
#' probabilities result in lower mobility rates. In addition, higher numbers
#' of successes are used to assign mobile students to schools that are farther
#' away from home school. Defaults to 0.5.
#'
#' @param .mob_option A string. Indicates the mobility simulation option.
#' The choices are \code{c("center", "np")}. The first choice, "center",
#' indicates that a large, population distribution will be generated and
#' median centered, students will be drawn from it, and afterward, some
#' proportion of students should be determined to be non-mobile. The second
#' choice, "np", indicates that students' mobility will be drawn directly
#' from a binomial distribution with known properties (namely that the
#' expected value of success is n * p). Those with higher successes will be
#' sent to schools farther away. To determine if schools are above or below the
#' current home school, draws will be randomly and uniformly multiplied by
#' -1 or 1.
#'
#' @param .pct_mobile A numeric scalar between 0 and 1. Gives the proportion of
#' mobile students to be saved in the "center" option of
#' \code{.mob_option}.
#'
#' @param .pop_size A large numeric scalar. Gives the size of the population
#' distribution to be constructed for \code{.mob_option = "center"}.
#' Defaults to 250000L.
#'
#' @param .output_details Logical. Indicates whether the sampling information
#' (including the number of draws and trials, the probability of success, etc.)
#' should be included in the output. If \code{FALSE}, only the mobility
#' assignment vector will be returned. Defaults to FALSE.
#'
#' @return This function returns a list (or vector) that varies depending on the
#' option selected in \code{.mob_option} and whether the simulation details
#' should be output. If details are requested, the list outputs the mobility
#' pattern vectors with length equal to \code{.draws}. The numbers given
#' are the number of schools away from the current school to which a mobile
#' child is assigned. For example, if the output vector looks like
#' \code{c(0, 0, -2)}, then the first two children are not mobile, while the
#' third moves two schools away below their current school in the school
#' residual matrix.
#' @export
#'
#' @examples \dontrun{
#'
#' simulate_mobility(50)
#'
#' }
simulate_mobility <-
  function(
    .draws,
    .max_dist = 1,
    .p_success = 0.5,
    .mob_option = c("center", "np"),
    .pct_mobile = 0.2,
    .pop_size = 250000L,
    .output_details = FALSE
  ) {

    ##--setup--##

    # match the .mob_option argument to the choice option, where
    # "center" is option 1 and "np", representing n * p, for the expected value
    # of successes, is option 2
    .mob_option <- match.arg(.mob_option, choices = c("center", "np"))



    ##--select option based on .mob_option--##

    if (.mob_option == "center") {

      ##--option 1: center--##

      # select from a large, approximately normal binomial distribution
      # with a roughtly 50/50 probability of success, center the distribution
      # around 0 (at the median), remove all non-mobile draws (i.e.,
      # throw out the 0s), sample uniformly from the centered distribution with
      # no 0s, and then, after mobility has been assigned, throw out a certain
      # proportion of mobile students.

      # procedure advantages: allows students to attend schools outside of the
      # one up/one down method, not too slow (even with 250k draws to form the
      # population distribution), centering makes sending schools to school -2
      # as easy as to school +2, allows a non-random proportion of mobile
      # students.

      # procedure disadvantages: I'm not entirely sure what the probability is
      # that a student would go to the next farther away school. like, what gets
      # a student to a school 3 away from me instead of a school 2 away from me?


      ## implement option 1:

      # create the number of trials as the absolute value of the
      # maximum distance (the number of schools above or below the current
      # school) that a student could go, multiplied by 2
      n_trials <- abs(.max_dist) * 2

      # sample from the distribution
      pop_x <- rbinom(n = .pop_size, size = n_trials, prob = .p_success)

      # median center the data
      x_center <- pop_x - median(pop_x)

      # remove 0s so that initially, all students are mobile
      x_remove <- x_center[!(x_center == 0)]

      # sample randomly and uniformly from the median-centered population-ish
      # distribution
      mob_assignment <- sample(x_remove, .draws)

      # set a certain percentage of students to be non-mobile by
      # setting the (first .draws * (1 - .pct_mobile)) values to 0
      mob_switch <- seq_len(.draws * (1 - .pct_mobile))

      # set the values given by mob_switch to 0
      mob_assignment[mob_switch] <- 0

      # finally, shuffle the order of the values in mob_assignment so that
      # not all the mobile students come from the end of the school ID list
      # note: here, sample doesn't change the overall elements of the vector,
      # rather it sorts them into a random permutation
      mob_assignment <- sample(mob_assignment)

      ## output option 1:
      if (.output_details) {
        sim_details <-
          list(
            mob_option = .mob_option,
            mob_switch = mob_switch,
            pop_dist = pop_x,
            pop_center = x_center,
            pop_remove = x_remove,
            pct_mobile = .pct_mobile,
            draws = x,
            n_trials = n_trials,
            p_success = .p_success
          )
      } else {
        sim_details <- NULL
      }


      # end of option 1, start of option 2
    } else if (.mob_option == "np") {


      ##--option 2: np--##

      # select only the final draws from a binomial distribution with size * p
      # (or n * p, if you aren't thinking about R functions) average successes.
      # assign students who have no (0) successes no mobility and students who
      # have 1 or more successes to the the school that is that number of
      # successes away from their current school. randomly, uniformly sample from
      # a -1/+1 vector with replacement to determine if that school is above or
      # below the current school in the n_sch x n_sch correlated residual matrix.

      # procedure advantages: there is a known but random average chance of
      # being mobile (n*p is the expected value), and the distribution hasn't
      # been weirdly manipulated such that the probability from going from one
      # success to two (i.e., a school 1 away vs a school 2 away) is still
      # solvable analytically.

      # procedure disadvantages: I think, but I don't know for certain, that this
      # procedure is too restrictive on allowing students to go far away from
      # their home school in terms of the school residual matrix. It's really
      # tough to be mobile, and beyond that, very tough to be mobile far from
      # home. To some extent, that's what we want, but it might also be too
      # restrictive.


      ## implement option 2:

      # create the number of trials as the absolute value of the
      # maximum distance (the number of schools above or below the current
      # school) that a student could go
      n_trials <- abs(.max_dist)

      # sample from the distribution
      x <- rbinom(n = .draws, size = n_trials, prob = .p_success)

      # sample randomly and uniformly from a c(-1, 1) vector with replacement
      # to identify if mobile students will move to a school above or below
      # their home school in the school residual matrix
      mob_switch <- sample(c(-1, 1), .draws, replace = TRUE)

      # multiply the mobility switch on/off sampler with the draws from
      # the binomial distribution
      mob_assignment <- x * mob_switch


      ##--output option 2:
      if (.output_details) {
        sim_details <-
          list(
            mob_option = .mob_option,
            mob_switch = mob_switch,
            draws = x,
            n_trials = n_trials,
            p_success = .p_success
          )
      } else {
        sim_details <- NULL
      }

    }


    ##--output--##
    if (is.null(sim_details)) {
      mob_assignment
    } else {
      list(
        mob_assignment = mob_assignment,
        sim_details = sim_details
      )
    }


  }
