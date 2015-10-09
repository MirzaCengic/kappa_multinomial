##' Multinomial models are difficult to assess in across--model way.  
##' 
##' @title Cohen's Kappa
##' 
##' @encoding utf8
##' 
##' @param obs a data.frame with class obervations with n columns and m rows; each row represents a sample, the columns represent the classes of outcomes. 
##' @param pred a data.frame with class predictions n columns and m rows; each rows represents a sample, the columns represent the classes of outcomes. 
##' 
##' @return returns a list with the following elements: 
##'  \itemize{
##' \item{Kappa}
##' }
##' 
##' @export
##' 
##' @details Method following Cohen (1960)
##' @author Bob Douma
##' 
##' @references 
##' NULL
##' @seealso 
##' NULL
##' 
##' @examples 
##' library(gtools)
##' class = c(1,0,0,0)
##' obs = t(replicate(100,sample(class)))
##' pred = t(replicate(100,sample(class)))
##' cohen_kappa(obs=obs,pred=pred) # calculate kappa
##' 

cohen_kappa<-function(obs,pred){
  pred.pa = prob_to_binary(pred)
  x = kappa_multinomial_stats(obs=obs,pred=pred.pa)
  obs_total<-x$observed                                        
  pred_total<-x$predicted
  realized<-x$realized
  pii<-realized/sum(obs_total) # fraction of correctly classified cells
  pi.<-obs_total/sum(obs_total) # marginal totals of observed
  p.i<-pred_total/sum(pred_total) # marginal totals of predicted
  pe = sum(pi.*p.i)
  k_spec<-(pii-pe)/(1-pe) 	# kappa calculation
  names(k_spec)<-c("Cohen_Kappa_overall")
  return(list(k_spec,po=pii,pe=pe))# returns kappa values
}
