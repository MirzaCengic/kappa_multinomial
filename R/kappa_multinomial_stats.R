# function to prepare data.frame for kappa_multinomial calculation

kappa_multinomial_stats = function(obs, pred,...){
  abs.diff = abs(obs - pred)
  po = sum(1-apply(abs.diff,1,sum)/2)/nrow(obs) # eq. 7 
  pmax = sum(1-apply(abs(t(apply(pred,1,sort))-t(apply(obs,1,sort))),1,sum)/2)/nrow(pred) # Equation 8 in the paper (it is called new because compared to the previous version of the paper; it should replace pmax when you agree on the method)
  
  return(c("po"=po,"pmax"=pmax))  
}

