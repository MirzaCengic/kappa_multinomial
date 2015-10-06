##' 
##' @title Kappa multinomial statistics
##' 
##' @description This function is typically called by kappa_multinomial and gathers the necessary statistics for producing kappa_multinomial
##' 
##' @encoding utf8
##' 
##' @param pred a dataframe 
##' @param obs a dataframe
##' @param ... other arguments to be passed to the underlying functions
##' 
##' @return returns a list with the following elements: 
##'  \itemize{
##' \item{observed} a data.frame with the sum of the class occurences
##' \item{predicted} a data.frame with the sum of the class probabilities 
##' \item{realized} a data.frame with the sum of the observed class probabilities
##' \item{pmax} a number with the average probability of the most probable class 
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

# function to prepare data.frame for kappa_multinomial calculation

kappa.multinomial.stats = function(obs, pred,...){
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

