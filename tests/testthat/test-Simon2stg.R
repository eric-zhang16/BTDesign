test_that("Simon 2-stage works", {

  n.s2stg <- 40
  n1.s2stg  <- 20
  p0.s2stg  <- 0.2
  p1.s2stg  <- 0.4
  r1.s2stg <- seq(1,20,1)
  r.s2stg  <- 40
  sum.2stage <- Simon2stg_design(n.s2stg,n1.s2stg,p0.s2stg ,p1.s2stg,r1.s2stg,r.s2stg, alpha=1-(1-0.1)^(1/4)  )
  test.picked <- sum.2stage$Picked
  res1 <-Simon2stg_analysis(n1.s2stg=n1.s2stg,n.s2stg=n.s2stg,r1.s2stg=test.picked$r1,r.s2stg=test.picked$r,r.ia=3,r=6)
  expect_equal( c(res1$rej,res1$totN ),c(0,40))

  res2 <-Simon2stg_analysis(n1.s2stg=n1.s2stg,n.s2stg=n.s2stg,r1.s2stg=test.picked$r1,r.s2stg=test.picked$r,r.ia=0,r=6)
  expect_equal(c(res2$rej,res2$totN ),c(0,20))

  res3 <-Simon2stg_analysis(n1.s2stg=n1.s2stg,n.s2stg=n.s2stg,r1.s2stg=test.picked$r1,r.s2stg=test.picked$r,r.ia=1,r=15)
  expect_equal(c(res3$rej,res3$totN ),c(0,20))

  res4 <-Simon2stg_analysis(n1.s2stg=n1.s2stg,n.s2stg=n.s2stg,r1.s2stg=test.picked$r1,r.s2stg=test.picked$r,r.ia=2,r=15)
  expect_equal(c(res4$rej,res4$totN ),c(1,40))


})
