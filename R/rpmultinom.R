# generate multinomial probabilities
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


