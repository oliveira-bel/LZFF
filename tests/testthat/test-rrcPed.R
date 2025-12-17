test_that("removing duplicate data", {
  df <- data.frame( S1 = c(2, 12, 12, 89,23), S2 = c(1, 12,12,13,49))
  rdd<-unique(df)
  expect_equal(nrow(rdd),4)
})

test_that("replacing absent parents", {
  df <- data.frame(id = 1:5,valor = c(10, NA, 30, NA, 50))
  d<-replace(df,list = is.na(df),values = 0)
  expect_equal(sum(d[[2]] == 0) ,2)
})

test_that("Checking if there are animal who appear in both columns: sire and dam",{
dadosPed<- data.frame(id = 1:5,valor = c(1, 8, 30, 78, 50))
bad<-match(dadosPed[,2], dadosPed[,1], incomparables = NA)
expect_error(if (!all(is.na(bad))){stop("Error: An animal appears in both columns: sire and dam. Please, check your pedigree. Function aborted!")
})
})



