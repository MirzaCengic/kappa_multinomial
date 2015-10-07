##'   
##' 
##' @title Multinomial distribution
##' 
##' @description This function generates a multinomial probilities for x classes
##' 
##' @encoding utf8
##' 
##' @param x the number of classes
##' @param skewness number to determine the differences between class probabilities, 1 < skewness < 100. The larger skewness the larger the difference between class probabilities
##' @param double logical; if TRUE, the difference between he most probable and second most probable class is very small
##' 
##' @return returns a list with the following elements: 
##'  \itemize{
##' \item{rpmultinom} generates random probabilities for x classes
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
##' package DirichletReg and gtools
##' 
##' @examples 
##' rpmultinom(10,skewness=1,double=FALSE)
##' 


rpmultinom<-function(x,skewness,double=FALSE){
  stopifnot(skewness >= 1 | skewness <= 100) 
# x number of classes to be predicted
# skewness values indicates the skewness of the generated #probabilities. If skewness low is one gets an even probability #distribution. If skewness is high, one gets an uneven probability #distribution 
# double: determines whether one other class is predicted with a probability close to the highest class.
  skewness =1/skewness # from 1:50
  # make sure that all classes will have prob > 0
  a = rnbinom(x,mu=100,size=skewness)+1 # generates integer values drawn from negative binomial distribution
  while (sum(a) <= x){
    a = rnbinom(x,mu=100,size=skewness)+1
  }
  if (double){ 
    vec = c(1:x)
    a[sample(vec[-which.max(a)],1)] = max(a)-1
  }
  # determines if there are multiple maxiumum values
  if (sum(a == max(a))>1){
    t.f = which(a == max(a))
    n.sample = length(t.f)-1
    index = sample(t.f,n.sample)
    a[index] = a[index]-2
  }
  a.f = frequency.a(a)
  return(a.f)
}


