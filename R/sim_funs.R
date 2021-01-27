####-------------------------------------
##
#    Simulation Loop Code
##
####-------------------------------------


cc.sim.loop <- function(dat.path,
                        sim.strt,
                        sim.end,
                        sch.strt,
                        sch.cond,
                        mob.strt,
                        mob.cond,
                        cor.strt,
                        cor.cond,
                        icc.strt,
                        icc.cond,
                        stu=nstu,
                        inner.loop.fun,
                        ...){

  for(j in sch.strt:sch.cond){
    if(j == 1){
      sch = 50  # number of schools
    } else if(j == 2){
      sch = 100  # number of schools
    } else if(j == 3){
      sch = 200  # number of schools
    } else if(j == 4){
      sch = 300   # number of schools
    }

    for(k in mob.strt:mob.cond){
      if(k == 1){
        mob = 0.00  #mobility rate
      } else if(k == 2){
        mob = 0.25  #mobility rate
      } else if(k == 3){
        mob = 0.50  #mobility rate
      }

      for(i in cor.strt:cor.cond){
        if(i == 1){
          clust.corr <- 0.8    #diagonal
          clust.corr1 <- 0.0   #1st off diagonal
          clust.corr2 <- 0.0   #2nd off diagonal
          cor <- 0.00

        } else if(i == 2){
          clust.corr <- 0.80    #diagonal
          clust.corr1 <- 0.50   #1st off diagonal
          clust.corr2 <- 0.20   #2nd off diagonal
          cor <- 0.50

        } else if(i == 3){
          clust.corr <- 0.80    #diagonal
          clust.corr1 <- 0.25   #1st off diagonal
          clust.corr2 <- 0.125   #2nd off diagonal
          cor <- 0.25

        }

        for(l in icc.strt:icc.cond){
          if(l == 1){
            x1beta <- sqrt(17)/2
            icc <- 0.05

          } else if(l == 2){
            x1beta <- sqrt(11/3)/2
            icc <- 0.15
          }

          else if(l == 3){
            x1beta <- 1/(2*sqrt(3))
            icc <- 0.30
          }

          condition <- paste0("-nsch", sch,
                              "-pct", mob,
                              "-cor", cor,
                              "-icc", icc,
                              "-nstu", stu)
          sub.path <- paste0("\\Cond", condition, "\\")
          sav.path <- paste0(dat.path, sub.path)

          dir.create(sav.path, showWarnings = FALSE)


          inner.args <- list(sch=sch,
                             mob=mob,
                             cor=cor,
                             icc=icc,
                             x1beta=x1beta,
                             stu=stu,
                             clust.corr=clust.corr,
                             clust.corr1=clust.corr1,
                             clust.corr2=clust.corr2,
                             dat.path=dat.path,
                             sub.path=sub.path,
                             sav.path=sav.path,
                             sim.strt=sim.strt,
                             sim.end=sim.end,
                             sch.strt=sch.strt,
                             sch.cond=sch.cond,
                             mob.strt=mob.strt,
                             mob.cond=mob.cond,
                             cor.strt=cor.strt,
                             cor.cond=cor.cond,
                             icc.strt=icc.strt,
                             icc.cond=icc.cond,
                             sch.ind=j,
                             mob.ind=k,
                             cor.ind=i,
                             icc.ind=l,
                             ...)

          #print(inner.args)

          do.call(what = inner.loop.fun, args = c(inner.args))



        }
      }
    }
  }
}






####-------------------------------------
##
#    Model Running
##
####-------------------------------------

##lives in cc.sim.loop wrapper
cc.mod.run.inner.loop <- function(dat.path,
                                  sub.path,
                                  sav.path,
                                  sim.strt,
                                  sim.end,
                                  form.strt,
                                  form.cond,
                                  sch.strt,
                                  sch.cond,
                                  mob.strt,
                                  mob.cond,
                                  cor.strt,
                                  cor.cond,
                                  icc.strt,
                                  icc.cond,
                                  mod.strt,
                                  mod.cond,
                                  stu,
                                  sch,
                                  mob,
                                  cor,
                                  icc,
                                  sch.ind,
                                  mob.ind,
                                  cor.ind,
                                  icc.ind,
                                  burn,
                                  iter,
                                  ...){

  library(R2MLwiN)
  res.path.seed <- "\\Results\\"
  dir.create(paste0(dat.path, res.path.seed), showWarnings = FALSE)

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

  res.path <- paste0(res.path.seed, "Res-",
                     "nsch", sch, "-",
                     "pct", mob, "-",
                     "cor", cor, "-",
                     "icc", icc, "-",
                     "nstu", stu)

  dir.create(paste0(dat.path, res.path),
             showWarnings = FALSE)


  write.table(results.coef.fp.int,
              paste(dat.path, res.path,
                    "\\results_coef_fp_int.txt", sep=""),
              row.names = FALSE)
  write.table(results.coef.rp.int.v2,
              paste(dat.path, res.path,
                    "\\results_coef_rp_int_v2.txt", sep=""),
              row.names = FALSE)
  write.table(results.coef.rp.int.v1,
              paste(dat.path, res.path,
                    "\\results_coef_rp_int_v1.txt", sep=""),
              row.names = FALSE)

  tryCatch(expr = {
    write.table(results.coef.fp.x1,
                paste(dat.path, res.path,
                      "\\results_coef_fp_x1.txt", sep=""),
                row.names = FALSE)
    write.table(results.coef.fp.compz,
                paste(dat.path, res.path,
                      "\\results_coef_fp_compz.txt", sep = ""),
                row.names = FALSE)
  }, error = function(e){ })


  write.table(results.sumstat,
              paste(dat.path, res.path,
                    "\\results_sumstat.txt", sep=""),
              row.names = FALSE)

}
