## ----global_options, include = FALSE----------------------------------------------------------------------------------
library(knitr)
options(width = 120)
opts_chunk$set(fig.width = 12, fig.height = 8, fig.path = 'Figs/',
               echo=TRUE,results="markup",eval=TRUE,
               include = TRUE, warning = FALSE, message = FALSE)

select_max <- function(x){
  t.f = x>=max(x)
  if (length(x[t.f])>1){
    y = rep(0,length=length(x))
    seq.1 = c(1:length(x))
    index = sample(seq.1[t.f],1)
    y[index] = 1
  } else  {
    y = ifelse(x>=max(x),1,0)  
  }
  return(y)
}


prob_to_binary = function(pred){
pred.type = t(apply(pred,1,select_max)) 
# transform maximum prob into 1 and others in 0's
  return(pred=pred.type)
}  
  

## ----eval = TRUE------------------------------------------------------------------------------------------------------
#install.packages("devtools")
devtools::install_github("bobdouma/kappa_multinomial")
library(multinomialperform)
#install.packages("gtools")
library(gtools)

## ----eval = TRUE------------------------------------------------------------------------------------------------------
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
pred = t(apply(pred,1,sample)) # randomly shuffles the columns for each sample; otherwise only one class is most likely
obs = as.data.frame(prob_to_binary(pred)) # prob_to_binary transforms probabilities to discrete outcomes, 1 for the most probable class, 0's for the remaining classes 
kappa_multinomial(obs=obs,pred=pred) # calculate kappa

## ----eval = TRUE------------------------------------------------------------------------------------------------------
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,4.5,0.5))) # generate multinomial probabilties with four classes
pred = t(apply(pred,1,sample))
obs = as.data.frame(prob_to_binary(pred))
kappa_multinomial(obs=obs,pred=pred) # calculate kappa

## ----eval = TRUE------------------------------------------------------------------------------------------------------
obs = as.data.frame(prob_to_binary(pred))  # transform probs to 0,1 (1 being the most likely)
resample = sample(c(1:100),20) # randomly pick 20 observations
obs[resample,] = obs[sample(resample),] # randomly shuffle 20 observations
kappa_multinomial(obs=obs,pred=pred) # calculate kappa

## ----eval = TRUE------------------------------------------------------------------------------------------------------
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs = pred
kappa_multinomial(obs=obs,pred=pred) # calculate kappa

## ----eval = TRUE------------------------------------------------------------------------------------------------------
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs = as.data.frame(t(apply(pred,1,sample))) # randomly shuffle observations
kappa_multinomial(obs=obs,pred=pred,nsim=10000) # calculate kappa

## ----eval = TRUE------------------------------------------------------------------------------------------------------
increase = function(x,val=0.05){
  for (i in 1:nrow(x)){
  x[i,which.max(x[i,])] =  x[i,which.max(x[i,])] - val
  x[i,-which.max(x[i,])] = x[i,-which.max(x[i,])]+   val/(ncol(x)-1)
  }
  return(x)
}

pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs = pred # randomly shuffle observations

pred.lower= increase(pred,val=0.1)

kappa_multinomial(obs=obs,pred=pred) # calculate kappa
kappa_multinomial(obs=obs,pred=pred.lower) # calculate kappa

