context("")

test_that("calculations sane", {
  pred <- gtools::rdirichlet(100, c(0.1,0.1,0.5,0.5)) # generate multinomial probabilties with four classes
  obs <- t(apply(pred,1,rmultinom,size=1,n=1))
  k_stats = kappa_multinomial_stats(obs=obs,pred=pred)
  expect_that(k_stats,is_a("list"))
  expect_true(k_stats$pmax<1 & k_stats$pmax>0)
  k_mn<-kappa_multinomial(k_stats) 
  expect_true(k_mn$k_prob<1 & k_mn$k_prob>0)
})


