##'   
##' @title select maximum
##' 
##' @description This function select the class with highest probability in a sample. If multiple classes have the same heighest probability then one is randomly chosen
##' 
##' 
##' @param x a vector with sample predictions: the elements represent the classes of outcomes. 
##' 
##' @return returns a vector with the following elements: 
##' \item{y}
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
