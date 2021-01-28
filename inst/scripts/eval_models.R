# # install from CRAN
# options(repos = c(CRAN = "https://cran.rstudio.com/"))
#
# # install corclus from private repo
# devtools::install_github(
#   "tessaleejohnson/corclus",
#   ref = "master",
#   auth_token = "a810f15d40eae26e7f92ab67747f428310f9c15f"
# )


library(corclus)
library(R2MLwiN)
library(bayesplot)
library(coda)

# debug inputs
.n_sch <- 5

# generate data
temp_dat <- generate_data()


# hlm example (ignoring multiple memberships)
hlm_mod <-
  run_mlwin(
    temp_dat,
    .model_formula = as.formula("y ~ 1 + (1 | sch_id_1) + (1 | stu_id)"),
    .mcmc_nchains = 2
  )

# multiple membership example (ignoring correlated clusters)
mm_mod <-
  run_mlwin(
    temp_dat,
    .model_formula = as.formula("y ~ 1 + (1 | ids_1) + (1 | stu_id)"),
    .mm_list = build_mm_list(),
    .mcmc_nchains = 2
  )




## conditions for simulation estimation

i <- 1
if (i == 1) {
  model.form <- Y ~ 1 + (1 | sch1) + (1 | stuID)
  frm <- "con00"
} else if (i == 2) {
  model.form <- Y ~ 1 + compZ + (1 | sch1) + (1 | stuID)
  frm <- "con02"
} else if (i == 3) {
  model.form <- Y ~ 1 + X1 + compZ + (1 | sch1) +
    (1 | stuID)
  frm <- "con12"
}

k <- 1
if (k == 1) {
  (mod.fit <-
     R2MLwiN::runMLwiN(model.form, D = dist,
              estoptions = list(
                EstM = 1,
                mcmcMeth = list(
                  burnin = burn, iterations = iter)),
              data = dat))
  mods <- "hlm"
} else if (k == 2) {
  (mod.fit <-
     R2MLwiN::runMLwiN(model.form, D = dist,
              estoptions = list(
                EstM = 1,
                mcmcMeth = list(burnin = burn, iterations = iter),
                mm = list(
                  list(mmvar = list("sch1", "sch2"),
                       weights = list("sch1.wt", "sch2.wt")), NA),
                resi.store = FALSE),
              data = dat))
  mods <- "mmm"
}




