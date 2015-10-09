##' Multinomial models are difficult to assess in across--model way.  
##' 
##' @title AUC multiclass
##' 
##' @encoding utf8
##' 
##' @param obs a data.frame with class obervations with n columns and m rows; each row represents a sample, the columns represent the classes of outcomes. 
##' @param pred a data.frame with class predictions n columns and m rows; each rows represents a sample, the columns represent the classes of outcomes. 
##' 
##' @return returns a list with the following elements: 
##'  \itemize{
##' \item{AUC for n classes}
##' \item{mAUC}
##' }
##' @import pROC
##' @export
##' 
##' @details method taken from Provost and Domingos 2001, Machine learning
##' @author Bob Douma
##' 
##' @references 
##' NULL
##' @seealso 
##' NULL
##' 
##' @examples 
##' library(gtools)
##' pred = rdirichlet(100, c(0.1,0.1,0.5,0.5)) # generate multinomial probabilties with four classes
##' obs = t(apply(pred,1,rmultinom,size=1,n=1)) # generate multinomial observations with four classes
##' AUC_multiclass(obs=obs,pred=pred)
##' 

AUC_multiclass = function(obs,pred){ 
# formula from Fawcett 2006 Pat Rec Letters. They took it from # Provost and Domingos 2001, Machine learning
  if (ncol(obs)!=ncol(pred)){stop("data.frames of unequal size")}
  if (nrow(obs)!=nrow(pred)){stop("data.frames of unequal size")}
  if (sum(apply(pred,1,sum)) != nrow(pred)){stop("rowsums not equal to one")}
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


