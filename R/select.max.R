# this function converts multinomial probability distribution to presence/absence. If multiple classes are predicted with the same maximum probability one class is chosen.

select.max = function(x){
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
