

##'   
##' @title prob_to_binary
##' 
##' @description This function transform probabilties to presence/absence, by assuming that the class with highest sample probability is present and the others classes are absent
##' 
##' 
##' @param pred a data.frame with class predictions n columns and m rows; each rows represents a sample, the columns represent the classes of outcomes. 
##' 
##' @return returns a list with the following elements: 
##' 
##' \itemize{
##' \item{pred}
##' }
##' 
##' @details helper function for the tutorial. This function tansforms probabilities to presence/absence, but assuming that the class that occurs with highest probability is present (1), and the others are absent (0). If multiple classes are predicted with the same maximum probability one class is chosen randomly.
##' 
##' @author Bob Douma
##' 
##' @references 
##' NULL
##' @seealso 
##' NULL
##' @export
##' 
##' @examples 
##' # generate multinomial probabilties with four classes
##' pred <- gtools::rdirichlet(100, c(0.1,0.1,0.5,0.5)) 
##' pred_discrete <- prob_to_binary(pred)
##' obs <- t(apply(pred,1,rmultinom,size=1,n=1)) # generate multinomial observations with four classes
##' multinomialperform::kappa_multinomial(obs=obs,pred=pred_discrete) # calculate kappa multinomial
##' 
##' 


prob_to_binary <- function(pred){
pred.type = t(apply(pred,1,select_max)) 
# transform maximum prob into 1 and others in 0's
  return(pred=pred.type)
}  
  
