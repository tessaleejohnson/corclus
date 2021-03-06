% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_xr_rnorm.R
\name{gen_xr_rnorm}
\alias{gen_xr_rnorm}
\title{gen_xr_rnorm}
\usage{
gen_xr_rnorm(.n_stu, .n_sch, .mean_x, .var_x, .mean_r, .var_r)
}
\arguments{
\item{.n_stu}{A numeric scalar. The number of students attending each
school. Note: this is not the total number of students in the dataset,
merely the number of students per school.}

\item{.n_sch}{Numeric scalar. Gives the total number of schools in the
dataset. The variance-covariance matrix for predictor z will have dimensions
\code{.n_sch x .n_sch}.}

\item{.mean_x}{Numeric scalar. The mean of the predictor, x.}

\item{.var_x}{Numeric scalar. The variance of the predictor, x.}

\item{.mean_r}{Numeric scalar. The mean of the person-level residual, r.}

\item{.var_r}{Numeric scalar. The variance of the person-level residual, r.}
}
\value{
This function returns a dataframe with two columns,
\code{x_predictor} and \code{r_residual}. The predictor, x, is a person-level
predictor of the person-level outcome, y (generated using
\code{\link{gen_y_mmrem}}). The person-specific error, or residual, r, and
the person-level predictor, x, are both continuous and normally distributed,
generated using the \code{\link[stats]{Normal}} function.
}
\description{
gen_xr_rnorm
}
\examples{
\dontrun{

gen_xr_rnorm(.n_stu = 5, .n_sch = 5)

}
}
