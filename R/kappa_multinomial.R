##'   
##' @title kappa_multinomial
##' 
##' @description A kappa-like measure for multinomial models
##' 
##' 
##' @param obs a data.frame with class obervations with n columns and m rows; each row represents a sample, the columns represent the classes of outcomes. 
##' @param pred a data.frame with class predictions n columns and m rows; each rows represents a sample, the columns represent the classes of outcomes. 
##' 
##' @return returns a list with the following elements: 
##'  \itemize{
##' \item{Kappa_prob}
##' \item{Kappa_loc}
##' \item{Kappa_multinomial}
##' \item{p0}
##' \item{pe}
##' \item{pmax}
##' }
##' 
##' @export 
##' 
##' @details Multinomial models are difficult to assess in across--model way.
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
##' kappa_multinomial(obs=obs,pred=pred) # calculate kappa multinomial
##' 
##' 
##' 
##' 

# kappa calculation
kappa_multinomial<-function(obs,pred,...){
  # do checks
  if (!is.data.frame(obs)){stop("observations not in a dataframe")}  
  if (!(dim(pred)[1] == dim(obs)[1] & dim(pred)[2] == dim(obs)[2])){stop("data.frames of unequal size")}
  if (sum(apply(pred,1,sum)) != nrow(pred)){stop("rowsums of predictions not equal to one")}
  if (sum(apply(obs,1,sum)) != nrow(obs)){stop("rowsums of observations not equal to one")}
  x = kappa_multinomial_stats(obs=obs,pred=pred)
  po = x["po"]      # observed agreement; Eq. 7
  pe = pe(obs)      # null model obtained through randomization or through the analytical; Eq. 9
  # marginal totals of predicted
  pmax = x["pmax"]  # Eq. 8
  k_prob<-(po-pe)/(pmax-pe) # Eq. 6
  k_loc<-(pmax-pe)/(1-pe) # Eq. 6
  k_multinomial <- k_loc*k_prob # # Eq. 6
  out = c(k_prob=k_prob,k_loc=k_loc,k_multinomial=k_multinomial,po = po,pe=pe,pmax=pmax)
  names(out) = c("k_prob","k_loc","k_multi","po","pe","pmax") 
  return(out) #returns kappa values
}


# function to prepare data.frame for kappa_multinomial calculation
kappa_multinomial_stats = function(obs, pred,...){
  abs.diff = abs(obs - pred)
  po = sum(1-apply(abs.diff,1,sum)/2)/nrow(obs) # eq. 7 
  pmax = sum(1-apply(abs(t(apply(pred,1,sort))-t(apply(obs,1,sort))),1,sum)/2)/nrow(pred) # Equation 8 in the paper (it is called new because compared to the previous version of the paper; it should replace pmax when you agree on the method)
  
  return(c("po"=po,"pmax"=pmax))  
}



