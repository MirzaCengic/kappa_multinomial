##' Multinomial models are difficult to assess in across--model way.  
##' 
##' @title A kappa-like measure for multinomial models
##' 
##' @encoding utf8
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
##' @details This is details
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
##' kappa_multinomial(obs=obs,pred=pred) # calculate kappa
##' 


# kappa calculation
kappa_multinomial<-function(obs,pred){
  x = kappa_multinomial_stats(obs=obs,pred=pred)
  obs_total<-x$observed                                        
  pred_total<-x$predicted
  realized<-x$realized
  po<-realized/sum(obs_total)                                          # fraction of correctly classified cells
  pi.<-obs_total/sum(obs_total)                                         # marginal totals of observed
  pe = sum(pi.*pi.)
  # marginal totals of predicted
  pmax = x$pmax
  k_prob<-(po-pe)/(pmax-pe)
  k_loc<-(pmax-pe)/(1-pe)
  k_multinomial <- k_loc*k_prob                                                                                                # kappa calculation
  return(list(k_prob=k_prob,k_loc=k_loc,k_multinomial=k_multinomial,po = po,pe=pe,pmax=pmax))   # returns kappa values
}
