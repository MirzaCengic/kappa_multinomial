##' Multinomial models are difficult to assess in across--model way.  
##' 
##' @title Performance statistics for multinomial models
##' 
##' @encoding utf8
##' 
##' @param i input, probably should generalize this
##' 
##' @param input figure out logical structure for this
##' 
##' @param ...  Additional arguments passed to ?
##' 
##' @return returns a list with the following elements: 
##'  \itemize{
##' \item{Kappa}
##' \item{some other kappa}
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
kappa_multinomial<-function(i,input,...){
  obs_total<-input[[i]]$observed                                        
  pred_total<-input[[i]]$predicted
  realized<-input[[i]]$realized
  po<-realized/sum(obs_total)                                          # fraction of correctly classified cells
  pi.<-obs_total/sum(obs_total)                                         # marginal totals of observed
  pe = sum(pi.*pi.)
  # marginal totals of predicted
  pmax = input[[i]]$pmax
  k_prob<-(po-pe)/(pmax-pe)
  k_loc<-(pmax-pe)/(1-pe)
  k_multinomial <- k_loc*k_prob                                                                                                # kappa calculation
  k = c(k_prob,k_loc,k_multinomial)
  names(k) = c("Kappa_prob","Kappa_loc","Kappa_multinomial")
  return(list(k[1],k[2],k[3],po = po,pe=pe,pmax=pmax))   # returns kappa values
}
