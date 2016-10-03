
# This is a brief tutorial for using `kappa_multinomial`, a new way to assess the performance of multinomial predictions for landcover


First install the packages.  Here we use the library `gtools` to generate some data to simulate data for testing purposes.

```{R,eval = TRUE}
#install.packages("devtools")
devtools::install_github("bobdouma/kappa_multinomial")
library(multinomialperform)
#install.packages("gtools")
library(gtools)
```


In the simplest case there is a matrix (or data frame) of predictions and a data frame of observations.  Both of these can either be binary or continuous probablities.  But all cells of both matrixes must be $0<=x<=1$.  The columns of both matrices correspond to $q$ land cover classes.  Each row is a sample $i$. Further for the modelled class probabilities and the class observations it should hold that $\sum_{k=1}^q y_{ik} =1$ and  $\sum_{k=1}^m p_{ik} = 1$.Not that one of the advances of this approach is that uncertainty in the observations can be represented.  The order of the rows for the land cover classes in the observed and predicted matrices should correspond exactly.    

### case 1: Predictions are continuous probabilities and observations discrete

In this case we show an example where the observed classes are predicted to be most likely. This implies that $\kappa_{prob}$ should equal to one and $\kappa_{loc}$ will depend on the average certainty with which the observed classes are predited

```{R,eval = TRUE}
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
pred = t(apply(pred,1,sample)) # randomly shuffles the columns for each sample; otherwise only one class is most likely
obs = as.data.frame(prob_to_binary(pred)) # prob_to_binary transforms probabilities to discrete outcomes, 1 for the most probable class, 0's for the remaining classes 
kappa_multinomial(obs=obs,pred=pred) # calculate kappa
```


### case 2: Predictions are continuous probabilities and observations discrete

In this case we show an example where the observed classes are predicted to be most likely. However, the certainty with which the classes are predicted is much higher.


```{R,eval = TRUE}
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,4.5,0.5))) # generate multinomial probabilties with four classes
pred = t(apply(pred,1,sample))
obs = as.data.frame(prob_to_binary(pred))
kappa_multinomial(obs=obs,pred=pred) # calculate kappa
```


### case 3: Predictions are continuous probabilities and observations discrete

In this case we show an example where there is mismatch between the observed classes and the classes predicted to be most likely. $\kappa_{loc}$ remains similar as the previous example. However $\kappa_{prob}$ becomes lower than one. 


```{R,eval = TRUE}
obs = as.data.frame(prob_to_binary(pred))  # transform probs to 0,1 (1 being the most likely)
resample = sample(c(1:100),20) # randomly pick 20 observations
obs[resample,] = obs[sample(resample),] # randomly shuffle 20 observations
kappa_multinomial(obs=obs,pred=pred) # calculate kappa
```


### case 4: Predictions are continuous probabilities and observations not discrete

In this case we show an example wehere prediction frequency is equal to observed frequency. In this case \kappa_{multinomial} should equal 1, indicating perfect model performance.   

```{R,eval = TRUE}
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs = pred
kappa_multinomial(obs=obs,pred=pred) # calculate kappa
```

### case 5: Observations not discrete and prediction frequency random relative to observed

In this case the evaluation of the model should find that $\kappa_{prob}$ preforms poorly, while the maximum model fit of a model with this set of modelled class distributions for the set of observations, $\kappa_{loc}$ is one.  

```{R,eval = TRUE}
pred = as.data.frame(rdirichlet(100, c(0.1,0.1,0.5,0.5))) # generate multinomial probabilties with four classes
obs = as.data.frame(t(apply(pred,1,sample))) # randomly shuffle observations
kappa_multinomial(obs=obs,pred=pred) # calculate kappa
```

### case 6: Observations not discrete and the most likely class to be observed is predicted with lower probability. However the ranking within a sample remains the same. 

Observations not discrete: The observations are generated such that the class predicted with higest probability is observed to be a bit less likely (val). The ranking of the sample probabilities remain the same; hence $\kappa_{prob}$ is close to one (not exactly because if the class that is predicted with highest probablity is smaller than the second highest class after subtracting val than the ranking is changed). In contrast $\kappa_{loc}$ decreases.


```{R,eval = TRUE}
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
```



