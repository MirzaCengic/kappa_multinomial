##'   
##' @title prob_to_binary
##' 
##' @description This function transform probabilties to presence absence, by assuming that the class with highest sample probability if present and the others classes are absent
##' 
##' @encoding utf8
##' 
##' @param pred a data.frame with class predictions n columns and m rows; each rows represents a sample, the columns represent the classes of outcomes. 
##' 
##' @return returns a list with the following elements: 
##' \item{pred}
##' 
##' @export 
##' 
##' @details helper function for the tutorial
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
##' pred.bin = prob_to_binary(pred)
##' 
##' 
##' 


prob_to_binary = function(pred){
pred.type = t(apply(pred,1,select_max)) 
# transform maximum prob into 1 and others in 0's
  return(pred=pred.type)
}  
  
