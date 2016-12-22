context("Sample CNJ process codes.")

test_that("sample_cnj is a set of random process codes.", {

 expect_equal(nrow(sample_cnj(3, foros = "0000",
                         anos = "2015", orgao = 8, tr = 26,
                         first_dig = "0",sample_pars = T, return_df = T)), 3)
  
 expect_error(sample_cnj(3, foros = c("0001","0000"),
                         anos = "2015", orgao = 8, tr = 26,
                         first_dig = "0",sample_pars = F, return_df = T))
 
 expect_error(sample_cnj(3, foros = c("0001","0000"), anos = c("2015","2014"),
                         orgao = c(8,7), tr = c(25,26), first_dig = "0",
                         sample_pars = F, return_df = T))
})