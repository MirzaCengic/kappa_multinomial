# function to prepare data.frame for kappa_multinomial calculation

kappa_multinomial_stats = function(obs, pred,...){
  abs.diff = abs(obs - pred)
  po = sum(1-apply(abs.diff,1,sum)/2)/nrow(obs)
  pmax = mean(apply(pred,1,max))
  pmax.test = mean(1-abs(apply(pred,1,max)- apply(obs,1,max)))
  pmax.new = sum(1-apply(abs(t(apply(pred,1,sort))-t(apply(obs,1,sort))),1,sum)/2)/nrow(pred)
  
  return(c("po"=po,"pmax"=pmax,"pmax.test"=pmax.test,"pmax.new"=pmax.new))  
}

