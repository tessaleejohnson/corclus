% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_data.R
\name{generate_data}
\alias{generate_data}
\title{generate_data}
\usage{
generate_data(
  .n_sch = 5,
  .n_stu = 5,
  .u_resid_var = 0.2,
  .clust_cov = 0.8,
  .wt_vec = c(0.5, 0.5),
  .pct_mobile = 0,
  .mean_x = 5,
  .var_x = 4,
  .mean_r = 0,
  .var_r = 2,
  .gamma_z = 0,
  .gamma_x = c(10, sqrt(17)/2),
  .mm_format = c("compact", "wide"),
  .progress_bar = NULL,
  ...
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

\item{.mm_format}{String. Options are "compact" or "wide".
Compact formats require two variables, one for the organization ID and
one for the weights; the number of variables in each type is equal to
the maximum number of organizational memberships for any individual in
the dataset. The wide format requires only one variable type, which
provides the weight for each person for each organization. MLwiN
documentation provides conflicting information about which type is required
by \code{\link[R2MLwiN]{runMLwiN}}. In the
\href{https://www.bristol.ac.uk/media-library/sites/cmm/migrated/documents/13-stata-example.pdf}{Stata documentation}.
it is said that both R and Stata require wide format. In the documentation
for the \code{R2MLwiN} \href{https://www.jstatsoft.org/article/view/v072i10/v72i10.pdf}{package}
multiple membership analysis is demonstrated with compact form. Here, we
use compact form as provided in the \code{R2MLwiN} package publication in
the Journal of Statistical Software.}

\item{.progress_bar}{Internal argument passed from \code{\link{run_sim}}.
If \code{.progress_bar = TRUE} in the external \code{\link{run_sim}}
function, then a progress bar will be displayed while executing this code.
The arguments in this internal function defaults to \code{NULL}.}

\item{...}{Other parameters passed to \code{\link{assign_mobility}}.}
}
\value{
This function returns data generated under the correlated cluster
multiple membership model, with correlated clusters defined by the
correlation between the school-level predictor z. The result is a tibble
with number of rows equal to \code{.n_sch * .n_stu} and a number of columns
equal to \code{18 + .n_sch}.
}
\description{
This function generates data according to the multiple membership
random effects model with correlated clusters given by the
variance-covariance matrix specified in \code{\link{gen_z_varcov}}.
}
\examples{
\dontrun{

# with the following defaults, we adjust x1beta to get the
# desired icc:

## icc = 0.05
# x1beta <- sqrt(17)/2

## icc = 0.15
# x1beta <- sqrt(11/3)/2

## icc = 0.30
x1beta <- 1/(2*sqrt(3))

generate_data(
  .n_sch = 5,
  .n_stu = 5,
  .u_resid_var = 0.2,
  .clust_cov = c(.8, 1),
  .wt_vec = c(0.5, 0.5),
  .pct_mobile = 0,
  .mean_x = 5,
  .var_x = 4,
  .mean_r = 0,
  .var_r = 2,
  .gamma_z = 0,
  .gamma_x = c(10, x1beta)
)

}
}
