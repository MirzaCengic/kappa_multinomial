##' Multinomial models are difficult to assess in across--model way.  
##' 
##' @title A kappa-like measure for multinomial models
##' 
##' @encoding utf8
##' 
##' @param x a list containing data summaries to be used in calculating kappa multinomial. These are usually the result of a call to kappa.multinomial.stats, but can be generated in another fasion.
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
##' 
##' NULL
##' 


# kappa calculation
kappa_multinomial<-function(x){
  obs_total<-x[[1]]$observed                                        
  pred_total<-x[[1]]$predicted
  realized<-x[[1]]$realized
  po<-realized/sum(obs_total)                                          # fraction of correctly classified cells
  pi.<-obs_total/sum(obs_total)                                         # marginal totals of observed
  pe = sum(pi.*pi.)
  # marginal totals of predicted
  pmax = x[[1]]$pmax
  k_prob<-(po-pe)/(pmax-pe)
  k_loc<-(pmax-pe)/(1-pe)
  k_multinomial <- k_loc*k_prob                                                                                                # kappa calculation
  k = c(k_prob,k_loc,k_multinomial)
  names(k) = c("Kappa_prob","Kappa_loc","Kappa_multinomial")
  return(list(k[1],k[2],k[3],po = po,pe=pe,pmax=pmax))   # returns kappa values
}
