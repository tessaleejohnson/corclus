% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plan_future.R
\name{plan_future}
\alias{plan_future}
\title{plan_future}
\usage{
plan_future(
  .plan = c("sequential", "multisession"),
  .n_cores = future::availableCores() - 1,
  ...
)
}
\arguments{
\item{.plan}{A call to \code{\link[future]{multisession}} or
\code{\link[future]{sequential}}.}

\item{.n_cores}{Numeric scalar or call to
\code{\link[future]{availableCores}}.}

\item{...}{Other parameters passed to the
\code{\link[future]{sequential}} or \code{\link[future]{multisession}}
strategies identified in \code{.plan}.}
}
\value{
Sets the parallel processing plan for resolving futures. See
\code{\link[future]{plan}} for output.
}
\description{
This function is a wrapper for \code{\link[future]{plan}} to set the
parallel processing plans for running simulations in R.
}
\examples{
\dontrun{

plan_future("multisession", 3)

}
}
