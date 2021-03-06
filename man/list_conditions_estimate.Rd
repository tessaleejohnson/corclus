% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_conditions_funs.R
\name{list_conditions_estimate}
\alias{list_conditions_estimate}
\title{list_conditions_estimate}
\usage{
list_conditions_estimate(
  .dat,
  .model_formula = lapply(list(NULL, "x_predictor", "z_composite", c("x_predictor",
    "z_composite")), function(v) write_formula(predictor_var = v)),
  .outcome_dist = "Normal",
  .mcmc_burn = 500,
  .mcmc_iter = 5000,
  .mcmc_nchains = 1,
  .mm_list = list(HLM = NA, MMREM = build_mm_list(.n_clust = 2))
)
}
\arguments{
\item{.dat}{A dataframe produced by \code{\link{generate_data}}.}

\item{.model_formula}{A formula object.}

\item{.outcome_dist}{A string. Defaults to "Normal". See the \code{D}
argument in \code{\link[R2MLwiN]{runMLwiN}}.}

\item{.mcmc_burn}{Numeric. Defaults to 100.}

\item{.mcmc_iter}{Numeric. Defaults to 1000.}

\item{.mcmc_nchains}{Numeric. Defaults to 1.}

\item{.mm_list}{A list. Generated by \code{\link{build_mm_list}}.}
}
\value{
This function returns a list of all combinations of arguments
passed to this function. Each list element is an argument with length
equal to the number of combinations.
}
\description{
This function wraps around \code{\link{list_conditions}} to expand all
combinations of the arguments passed to the function. The arguments can
then be passed to \code{purrr::pmap} (\code{\link[purrr]{map2}}) to iterate
over all simulation conditions.
}
\details{
NOTE: All arguments that take objects with length > 1 must
wrap those objects in a list.
Bad: \code{list_conditions_datagen(.clust_cov = c(0.8, 0))}.
Good: \code{list_conditions_datagen(.clust_cov = list(c(0.8, 0)))}.

Bad: \code{list_conditions_estimate(.dat = generate_data())}.
Good: \code{list_conditions_estimate(.dat = list(generate_data()))}.

The same applies to the \code{.model_formula} and \code{.mm_list} args,
even when only "one" condition is present. (That is, a single formula
with both a LHS and a RHS has length 3: "~", "LHS" and "RHS"; similarly,
for one multiple membership condition, \code{\link{build_mm_list}}
produces a list with length 2).
}
\examples{
\dontrun{

list_conditions_estimate()

}
}
