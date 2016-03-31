# function to calculate pe; Douma et al Kappa multinomial

pe = function(obs,nsim=1000){
# do checks on dataframe
  if (length(which(obs ==1 | obs ==0))<(nrow(obs)*ncol(obs))){print("Observations are not discrete. A randomization procedure is used to calculate pe. Consider adjusting nsim")}
  if (length(which(obs ==1 | obs ==0)) == (nrow(obs)*ncol(obs))){
    # analytical procedure
    glob.means = apply(obs,2,mean)
    mean.null = data.frame(matrix(NA,nrow=(nrow(obs)),ncol=ncol(obs)))
    for (i in 1:nrow(mean.null)){mean.null[i,] = glob.means}  
    
    abs.diff = abs(mean.null - obs)
    po.eq8 = sum(1-apply(abs.diff,1,sum)/2)/nrow(obs) # calculated according to equation 8 in paper. 
    pe = po.eq8
    } else {
      # set progressbar
      pb <- txtProgressBar(min=1,max=nsim)
      
      # randomization procedure
      pe.sim = numeric(nsim)
      for (i in 1:nsim){
        sample = sample(c(1:nrow(obs)))
        null.sample = obs[sample,]
        pe.sim[i] = sum(1-apply(abs(obs - null.sample),1,sum)/2)/nrow(obs)
       
        setTxtProgressBar(pb, i)
      }
      pe = mean(pe.sim)
    }
  
  return(c("pe"=pe))  
}

