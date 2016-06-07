rm(list=ls(all=TRUE))
# 
devtools::install_github("bobdouma/kappa_multinomial")
 
library(gtools)


# pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
# obs = as.data.frame(t(apply(pred,1,rmultinom,size=1,n=1))) # generate multinomial observations with four classes
# kappa_multinomial(obs=obs,pred=pred) # calculate kappa


# case 1: Observations not discrete: prediction frequency equal to oberved frequency
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs = pred
a = kappa_multinomial(obs=obs,pred=pred) # calculate kappa

sum(1-apply(abs(pred-obs),1,sum)/2)/nrow(pred)


pmax = sum(1-apply(abs(t(apply(pred,1,sort))-t(apply(obs,1,sort))),1,sum)/2)/nrow(pred)

# case 2: Observations not discrete: prediction frequency random to observed
pred.case2 = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs.case2 = as.data.frame(t(apply(pred,1,sample)))
b = kappa_multinomial(obs=obs.case2,pred=pred.case2) # calculate kappa


# case 3: Observations not discrete: The observations are generated such that the class predicted with higest probability is observed to be a bit less likely (val) 
# here I expect that the old method of calculating pmax will fail because (po-pe) > (pmax-pe) 
increase = function(x,val=0.1){
  for (i in 1:nrow(x)){
  x[i,which.max(x[i,])] =  x[i,which.max(x[i,])] - val
  x[i,-which.max(x[i,])] = x[i,-which.max(x[i,])]+   val/(ncol(x)-1)
  }
  return(x)
}

pred.case3= increase(pred,val=0.1)

kappa_multinomial(obs=obs,pred=pred.case3) # calculate kappa

### Note that k_prob > 1 when using the old method of calculating pmax
### Note that k_prob ~ 1 when using the new method. This makes sense. The model is right in predicting the ranking of the most likely classes; however it is wrong in predicting the accurate probability to each class. 




