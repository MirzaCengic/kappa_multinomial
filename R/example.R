# # Comparison of Kappa-star and ROC-AUC multiple class
# setwd("D:\\BobDouma\\Paper_vegtype_validation\\manuscript\\git\\Rscripts\\script")
# 
# source("mAUC.R")
# source("prob.to.binary.R")
# source("rpmultinom.R")
# source("select.max.R")
# source("cohen_kappa.R")
# source("data.frame.to.kappa.pars.R")
# source("frequency_a.R")
# source("kappa_multinomial.R")
# 
# # make multinomial models
# nclass = 10
# nrows = 1000
# i =1 
# pred<-t(replicate(nrows,rpmultinom(nclass,skewness=10,double=FALSE)))
# observed = Probabilistic.to.Binary(i,inform)
# 
# # combine observations and predictions to one list
# obs = observed
# pred =inform
# 
# # apply metrics
# converted_raster<-lapply(1, data.frame.to.kappa.pars, fact=1, obs=obs, pred=pred,perVegtype=FALSE)
# # calculate kappa
# kappa_output<-lapply(1,kappa_multinomial,converted_raster,perVegtype=FALSE)
# # calculate AUC
# AUC.output = AUC.multiclass(obs=obs,pred=pred)
# 
# pred1 = Probabilistic.to.Binary(i,pred)
# converted_raster.cohen<-lapply(1, data.frame.to.kappa.pars, fact=1, obs=obs, pred=pred1,perVegtype=FALSE)
# kappa_output.cohen<-lapply(1,cohen_kappa,converted_raster.cohen,perVegtype=FALSE)
# 
# 
