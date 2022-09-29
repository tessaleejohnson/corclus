#------------------------------------------------
# External Dependencies:
#
# Internal Dependencies:
#------------------------------------------------

#' simulate_mobility_TEST
#'
#' Testing out a new way to simulate mobility.
#'
#' @param .n_sch
#'
#' @return Matrix.
#' @export
#'
#' @examples \dontrun{
#'
#'
#'
#' }
simulate_mobility_TEST <-
  function(
    .n_sch,
    .n_stu
  ) {

    # STEP 1: create correlated clusters

    # STEP 2: divide students up into three groups - non-mobile, upwardly
    # mobile, downwardly mobile

    n_non <- 10
    n_up <- 10
    n_down <- 10

    # generate number of switches for n_up and n_down (shift the distribution
    # one to the right such that there are no people with 0 switches conditional
    # on being in a mobile group)
    n_up_switch <- rnbinom(n = n_up, size = 1, prob = 0.95) + 1
    n_down_switch <- rnbinom(n = n_down, size = 1, prob = 0.65) + 1

    # STEP 3: create good schools and bad schools and then...multinomial stuff



  }
