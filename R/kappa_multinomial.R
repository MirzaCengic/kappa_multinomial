##' Multinomial models are difficult to assess in across--model way.  
##' 
##' @title A kappa-like measure for multinomial models
##' 
##' @encoding utf8
##' 
##' @param x a list containing data summaries to be used in calculating kappa multinomial. These are usually the result of a call to kappa_multinomial_stats, but can be generated in another fasion.
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
##' k_stats = kappa_multinomial_stats(obs=obs,pred=pred) # REMOVE?????
##' kappa_multinomial(k_stats) # calculate kappa
##' 


# kappa calculation
kappa_multinomial<-function(x){
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
