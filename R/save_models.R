#---------------------------------
# External Dependencies:
# R2MLwiN
#
# Internal Dependencies:
#
#---------------------------------



save_models <-
  function(



  ) {


    names.coef <- c("nsim", "mod", "form", "nsch", "nstu", "pctmob", "cor", "icc", "est", "se", "stat", "p", "lwr", "upr", "ess")
    names.sumstat <- c("nsim", "mod", "form", "nsch", "nstu", "pctmob", "cor", "icc", "Dbar", "Dthetabar", "pD", "DIC", "N")

    mat.row <- (mod.cond*form.cond*sch.cond*mob.cond*cor.cond*icc.cond*sim.end)
    mat <- matrix(data=NA, nrow=mat.row, ncol=length(names.coef))

    results.coef.fp.int <- mat
    results.coef.fp.x1 <- mat
    results.coef.fp.compZ <- mat
    results.coef.rp.int.v2 <- mat
    results.coef.rp.int.v1 <- mat

    results.sumstat <- matrix(data=NA, nrow=mat.row, ncol=length(names.sumstat))

    colnames(results.coef.fp.int) <- names.coef
    colnames(results.coef.fp.x1) <- names.coef
    colnames(results.coef.fp.compZ) <- names.coef
    colnames(results.coef.rp.int.v2) <- names.coef
    colnames(results.coef.rp.int.v1) <- names.coef

    colnames(results.sumstat) <- names.sumstat


    dist <- "Normal"
    index <- 1
    for(j in sim.strt:sim.end){

      dat <- read.table(paste0(dat.path,
                               sub.path,
                               "cc", j, ".txt"),
                        header = TRUE)

      for(i in form.strt:form.cond){
        if(i == 1){
          ## fully unconditional
          model.form <- Y ~ 1 + (1 | sch1) + (1 | stuID)
          frm <- "con00"
        } else if(i == 2){
          ## level-2 covariate only (explains 80% of sch var & 0% or 25% or 50% of school correlation)
          model.form <- Y ~ 1 + compZ + (1 | sch1) + (1 | stuID)
          frm <- "con02"
        } else if(i == 3){
          ## data generating model, level-1 & level-2 covariates
          model.form <- Y ~ 1 + X1 + compZ + (1 | sch1) + (1 | stuID)
          frm <- "con12"
        }

        for(k in mod.strt:mod.cond){
          if(k == 1){
            (mod.fit <- runMLwiN(model.form, D=dist,
                                 estoptions = list(EstM = 1, mcmcMeth = list(burnin = burn, iterations = iter)),
                                 data = dat))
            mods <- "hlm"
          } else if(k == 2){
            (mod.fit <- runMLwiN(model.form, D=dist,
                                 estoptions = list(EstM = 1, mcmcMeth = list(burnin = burn, iterations = iter),
                                                   mm = list(list(mmvar = list("sch1", "sch2"),
                                                                  weights = list("sch1.wt", "sch2.wt")), NA),
                                                   resi.store = FALSE),
                                 data = dat))
            mods <- "mmm"
          }

          condition.deets <- c(j, k, i, sch, stu, mob, cor, icc)
          na.rep <- rep(NA, length(names.coef) - length(condition.deets))

          results.coef.fp.int[index, ] <- c(condition.deets, getSummary(mod.fit)$coef["FP_Intercept", ])
          results.coef.rp.int.v2[index, ] <- c(condition.deets, getSummary(mod.fit)$coef["RP2_var_Intercept", ])
          results.coef.rp.int.v1[index, ] <- c(condition.deets, getSummary(mod.fit)$coef["RP1_var_Intercept", ])


          if(i == 3){
            results.coef.fp.x1[index, ] <- c(condition.deets, getSummary(mod.fit)$coef["FP_X1", ])
            results.coef.fp.compZ[index, ] <- c(condition.deets, getSummary(mod.fit)$coef["FP_compZ", ])
          } else if(i == 2){
            results.coef.fp.x1[index, ] <- c(condition.deets, na.rep)
            results.coef.fp.compZ[index, ] <- c(condition.deets, getSummary(mod.fit)$coef["FP_compZ", ])
          } else if(i == 1){
            results.coef.fp.x1[index, ] <- c(condition.deets, na.rep)
            results.coef.fp.compZ[index, ] <- c(condition.deets, na.rep)
          }

          results.sumstat[index, ] <- c(condition.deets, getSummary(mod.fit)$sumstat)


          index <- index + 1

        }

      }

    }


  }


