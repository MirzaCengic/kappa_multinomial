# function to prepare data.frame for kappa_multinomial calculation

kappa_multinomial_stats = function(obs, pred,...){
  obs.agg.dat = obs
  pred.agg.dat = pred
  observed<-apply(obs.agg.dat,2,sum,na.rm=TRUE) 
  # calculates marginal totals
  predicted<-apply(pred.agg.dat,2,sum,na.rm=TRUE)
  common.freq<-(1-(apply(abs(obs.agg.dat-pred.agg.dat),1,sum)/2)) #calculates the fraction of cells in common between pred and obs      
  realized<-sum(common.freq,na.rm=TRUE)                           #number of cells in common on total raster
  pmax = mean(apply(pred.agg.dat,1,max))
  return(list(observed=observed, predicted=predicted,realized=realized,pmax=pmax))  
}

