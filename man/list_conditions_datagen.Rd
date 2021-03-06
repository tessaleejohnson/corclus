% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_conditions_funs.R
\name{list_conditions_datagen}
\alias{list_conditions_datagen}
\title{list_conditions_datagen}
\usage{
list_conditions_datagen(
  .n_sch = 50,
  .n_stu = 50,
  .u_resid_var = 0.2,
  .clust_cov = list(c(0.8, 0), c(0.8, 0.2), c(0.8, 0.4)),
  .wt_vec = list(c(0.5, 0.5)),
  .pct_mobile = c(0, 0.25, 0.5),
  .mean_x = 5,
  .var_x = 4,
  .mean_r = 0,
  .var_r = 2,
  .gamma_z = 1,
  .gamma_x = list(c(10, sqrt(17)/2), c(10, sqrt(11/3)/2), c(10, 1/(2 * sqrt(3))))
)
}
\arguments{
\item{.n_sch}{Numeric scalar. Gives the total number of schools in the
dataset. The variance-covariance matrix for predictor z will have dimensions
\code{.n_sch x .n_sch}.}

\item{.n_stu}{A numeric scalar. The number of students attending each
school. Note: this is not the total number of students in the dataset,
merely the number of students per school.}

\item{.u_resid_var}{Numeric scalar. Gives the residual variance of u0j (i.e.,
the variance unexplained after controlling for the school-level predictor,
z).}

\item{.clust_cov}{Numeric vector. The first element of the vector gives the
variance of all schools' predictors, z. If present, the second element gives
the covariance of z between schools k and k + 1. The values given in
\code{.clust_cov} apply to all schools (that is, similar to a Toeplitz
pattern). Any off-diagonal values (i.e., covariances) not specified will
default to 0. The main diagonal is the variance explained by the predictor.}

\item{.wt_vec}{A numeric vector with length equal to the maximum number
of schools attended by students in the data (in this simulation, the
maximum number is 2). The values in \code{.wt_vec} are used to weight
the effects of different schools attended on students. For this study,
all mobile students must have the same weights. If different weighting
patterns are desired, the code will need to be updated.}

\item{.mean_x}{Numeric scalar. The mean of the predictor, x.}

\item{.var_x}{Numeric scalar. The variance of the predictor, x.}

\item{.mean_r}{Numeric scalar. The mean of the person-level residual, r.}

\item{.var_r}{Numeric scalar. The variance of the person-level residual, r.}

\item{.gamma_z}{Numeric scalar. The school-level effect of the
\code{z_predictors} on the random intercept.}

\item{.gamma_x}{Numeric vector with length p (where p is the number of model
coefficients, including the intercept).}
}
\value{
This function returns a list of all combinations of arguments
passed to this function. Each list element is an argument with length
equal to the number of combinations.
}
\description{
Wraps around \code{\link{list_conditions}} to expand all combinations of
the arguments passed to the function. The arguments can
then be passed to \code{purrr::pmap} (\code{\link[purrr]{map2}}) to iterate
over all simulation conditions.
}
\details{
NOTE: All arguments that take objects with length > 1 must
wrap those objects in a list.
Bad: \code{list_conditions_datagen(.clust_cov = c(0.8, 0))}.
Good: \code{list_conditions_datagen(.clust_cov = list(c(0.8, 0)))}.
}
\examples{
\dontrun{

list_conditions_datagen(.nsch = 5, .pct_mobile = 0.1)

}
}
