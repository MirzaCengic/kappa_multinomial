kappa_Cohen<-function(i,input,perVegtype,...){
  obs_total<-input[[i]]$observed                                        
  pred_total<-input[[i]]$predicted
  realized<-input[[i]]$realized
  pii<-realized/sum(obs_total) # fraction of correctly classified cells
  pi.<-obs_total/sum(obs_total) # marginal totals of observed
  p.i<-pred_total/sum(pred_total) # marginal totals of predicted
  pe = sum(pi.*p.i)
  k_spec<-(pii-pe)/(1-pe) 	# kappa calculation
  names(k_spec)<-c("Cohen_Kappa_overall")
  return(list(kappa_cohen,po=pii,pe=pe))# returns kappa values
}
