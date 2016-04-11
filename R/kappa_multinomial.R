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
##' # function to prepare data.frame for kappa_multinomial calculation

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
##' kappa_multinomial(obs=obs,pred=pred) # calculate kappa multinomial
##' 


# kappa calculation
kappa_multinomial<-function(obs,pred,...){
  # do checks
  if (!is.data.frame(obs)){stop("observations not in a dataframe")}  
  if (!(dim(pred)[1] == dim(obs)[1] & dim(pred)[2] == dim(obs)[2])){stop("data.frames of unequal size")}
  if (sum(apply(pred,1,sum)) != nrow(pred)){stop("rowsums of predictions not equal to one")}
  if (sum(apply(obs,1,sum)) != nrow(obs)){stop("rowsums of observations not equal to one")}
  
  x = kappa_multinomial_stats(obs=obs,pred=pred)
  obs_total<-x$observed                                        
  pred_total<-x$predicted
  realized<-x$realized
  po = realized/sum(obs_total)                                          # fraction of correctly classified cells
  pe = pe(obs)      # null model
  # marginal totals of predicted
  pmax = x$pmax
  k_prob<-(po-pe)/(pmax-pe)
  k_loc<-(pmax-pe)/(1-pe)
  k_multinomial <- k_loc*k_prob                                                                                                # kappa calculation
  return(list(k_prob=k_prob,k_loc=k_loc,k_multinomial=k_multinomial,po = po,pe=pe,pmax=pmax))   # returns kappa values
}
