devtools::install_github("bobdouma/kappa_multinomial")


library(multinomialperform)
library(DirichletReg)


library(gtools)
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs = as.data.frame(t(apply(pred,1,rmultinom,size=1,n=1))) # generate multinomial observations with four classes
kappa_multinomial(obs=obs,pred=pred) # calculate kappa

source("D://BobDouma//Paper_vegtype_validation//ROC_AUC_multiple_class.r")
source("D://BobDouma//Paper_vegtype_validation//Aggregate_function_Kappa_star_apply.r")
source("D://BobDouma//Paper_vegtype_validation//multinomial_distribution_generator.r")
source("D://BobDouma//Paper_vegtype_validation//Cohen_Kappa.r")


obs = as.data.frame(Probabilistic.to.Binary(i,pred))

kappa_multinomial(obs,pred)

# pmax


# case 1: prediction frequency equal to oberved frequency
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs = pred
a = kappa_multinomial(obs=obs,pred=pred) # calculate kappa

sum(1-apply(abs(pred-obs),1,sum)/2)/nrow(pred)


pmax = sum(1-apply(abs(t(apply(pred,1,sort))-t(apply(obs,1,sort))),1,sum)/2)/nrow(pred)

# case 2: prediction frequency random to observed
pred1 = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs1 = as.data.frame(t(apply(pred,1,sample)))
b = kappa_multinomial(obs=obs1,pred=pred1) # calculate kappa

sum(1-apply(abs(pred1-obs1),1,sum)/2)/nrow(pred1)
# pmax
sum(1-apply(abs(t(apply(pred1,1,sort))-t(apply(obs1,1,sort))),1,sum)/2)/nrow(pred1)


# case 3
increase = function(x,val=0.1){
  for (i in 1:nrow(x)){
  x[i,which.max(x[i,])] =  x[i,which.max(x[i,])] - val
  x[i,-which.max(x[i,])] = x[i,-which.max(x[i,])]+   val/(ncol(x)-1)
  }
  return(x)
}

pred2= increase(pred)   

sum(1-apply(abs(pred2-obs),1,sum)/2)/nrow(pred2)
# pmax
sum(1-apply(abs(t(apply(pred2,1,sort))-t(apply(obs,1,sort))),1,sum)/2)/nrow(pred2)


aa = apply(t(apply(pred2,1,sort)),2,mean)
ab = apply(t(apply(obs,1,sort)),2,mean)
sum(pmin(aa,ab))


kappa_multinomial(obs=obs,pred=pred2) # calculate kappa
obs

# case 4 
# sample rows

sample.row = function(x,n=20){
  index = sample(c(1:nrow(x)),size=n)
  y = t(apply(x[index,],1,sample))
  x[index,] = y
  return(x)
}


pred4= pred2
pred4[c(1:100),] =  t(apply(pred2[c(1:100),],1,sample))


sum(1-apply(abs(pred4-obs),1,sum)/2)/nrow(pred4)
# pmax
sum(1-apply(abs(t(apply(pred4,1,sort))-t(apply(obs,1,sort))),1,sum)/2)/nrow(pred4)


kappa_multinomial(obs=obs,pred=pred4) # calculate kappa





