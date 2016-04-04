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
##' @export kappa_multinomial_stats
##' 
##' # function to prepare data.frame for kappa_multinomial calculation

kappa_multinomial_stats = function(obs, pred,...){
  abs.diff = abs(obs - pred)
  po = sum(1-apply(abs.diff,1,sum)/2)/nrow(obs)
  pmax = mean(apply(pred,1,max))
  pmax.test = mean(1-(apply(pred,1,max)- apply(obs,1,max)))
  return(c("po"=po,"pmax"=pmax,"pmax.test"=pmax.test))  
}

pe = function(obs,nsim=1000){
  # do checks on dataframe
  if (length(which(obs ==1 | obs ==0))<(nrow(obs)*ncol(obs))){print("Observations are not discrete. A randomization procedure is used to calculate pe. Consider adjusting nsim")}
  if (length(which(obs ==1 | obs ==0)) == (nrow(obs)*ncol(obs))){
    # analytical procedure
    glob.means = apply(obs,2,mean)
    mean.null = data.frame(matrix(NA,nrow=(nrow(obs)),ncol=ncol(obs)))
    for (i in 1:nrow(mean.null)){mean.null[i,] = glob.means}  
    
    abs.diff = abs(mean.null - obs)
    po.eq8 = sum(1-apply(abs.diff,1,sum)/2)/nrow(obs) # calculated according to equation 8 in paper. 
    pe = po.eq8
  } else {
    # set progressbar
    pb <- txtProgressBar(min=1,max=nsim)
    
    # randomization procedure
    pe.sim = numeric(nsim)
    for (i in 1:nsim){
      sample = sample(c(1:nrow(obs)))
      null.sample = obs[sample,]
      pe.sim[i] = sum(1-apply(abs(obs - null.sample),1,sum)/2)/nrow(obs)
      
      setTxtProgressBar(pb, i)
    }
    pe = mean(pe.sim)
  }
  
  return(c("pe"=pe))  
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
  po = x["po"]
  pe = pe(obs,...)      # null model
  # marginal totals of predicted
  pmax = x["pmax"]
  k_prob<-(po-pe)/(pmax-pe)
  k_loc<-(pmax-pe)/(1-pe)
  k_multinomial <- k_loc*k_prob# kappa calculation
  out = c(k_prob,k_loc,k_multinomial,po,pe,pmax)
  names(out) = c("k_prob","k_loc","k_multinomial","po","pe","pmax")
  # test
  pmax.test = x["pmax.test"]
  k_prob.test<-(po-pe)/(pmax.test-pe)
  k_loc.test<-(pmax.test-pe)/(1-pe)
  k_multinomial.test <- k_loc.test*k_prob.test# kappa calculation
  out.test = c(k_prob.test,k_loc.test,k_multinomial.test,po,pe,pmax)
  names(out.test) = c("k_prob","k_loc","k_multinomial","po","pe","pmax")
  return(list(out,out.test))   # returns kappa values
}
