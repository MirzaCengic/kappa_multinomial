# This function apply the function select.max to a data.frame consisting of multiple rows.
Probabilistic.to.Binary = function(i, pred,...){
pred.type = t(apply(pred,1,select.max)) 
# transform maximum prob into 1 and others in 0's
  return(pred=pred.type)
}  
  
