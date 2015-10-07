AUC.multiclass = function(obs,pred){ 
# formula from Fawcett 2006 Pat Rec Letters. They took it from # Provost and Domingos 2001, Machine learning
  #library(pROC)
  if (ncol(obs)!=ncol(pred)){stop}
  auc.all = list()
  roc.all = list()
  for (i in 1:ncol(obs)){
    prob.2class = data.frame(pred[,i],apply(pred[,-i],1,sum))
    obs.2class = data.frame(obs[,i],apply(obs[,-i],1,sum))
    roc.all[[i]] = pROC::roc(response = obs.2class[,1], predictor = prob.2class[,1])
    auc.all[[i]] = pROC::auc(pROC::roc(response = obs.2class[,1], predictor = prob.2class[,1]))[1]
    print(i)
  }
  auc.mean = weighted.mean(unlist(auc.all),w=apply(obs,2,sum))
  return(list(roc.all,AUC.mean=auc.mean))
}


