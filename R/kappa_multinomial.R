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
  pmax = x["pmax"]  # Eq. 4
  k_prob<-(po-pe)/(pmax-pe) # Eq. 6
  k_loc<-(pmax-pe)/(1-pe) # Eq. 6
  k_multinomial <- k_loc*k_prob # # Eq. 6
  # new 
  pmax.new = x["pmax.new"] # Eq. 8 Will, this refers to the new way that I calculate pmax. Originally, it iwas the maximum of the sample probabilities
  k_prob.new<-(po-pe)/(pmax.new-pe)
  k_loc.new<-(pmax.new-pe)/(1-pe)
  k_multinomial.new <- k_loc.new*k_prob.new# kappa calculation
  old = c(k_prob=k_prob,k_loc=k_loc,k_multinomial=k_multinomial,po = po,pe=pe,pmax=pmax)
  new = c(k_prob.new=k_prob.new,k_loc.new=k_loc.new,k_multinomial.new=k_multinomial.new,po = po,pe=pe,pmax.new=pmax.new)
  names(old) = c("k_prob","k_loc","k_multi","po","pe","pmax") 
  names(new) = c("k_prob","k_loc","k_multi","po","pe","pmax")
  return(list(old,new)) #returns kappa values
}


# function to prepare data.frame for kappa_multinomial calculation
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

# This function apply the function select.max to a data.frame consisting of multiple rows.
prob_to_binary = function(pred){
pred.type = t(apply(pred,1,select_max)) 
# transform maximum prob into 1 and others in 0's
  return(pred=pred.type)
}  
  

# this function converts multinomial probability distribution to presence/absence. If multiple classes are predicted with the same maximum probability one class is chosen.
select_max = function(x){
  t.f = x>=max(x)
  if (length(x[t.f])>1){
    y = rep(0,length=length(x))
    seq.1 = c(1:length(x))
    index = sample(seq.1[t.f],1)
    y[index] = 1
  } else  {
    y = ifelse(x>=max(x),1,0)  
  }
  return(y)
}
