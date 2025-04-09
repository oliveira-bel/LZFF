test_that("reading files working csv", {
  readFile<-rrcData(testthat::test_path("dataForTests", "DP-CA.csv"),
                    s = ",", d = ".", colsPed = 1, colsTraits = 4:8)
  expect_equal(ncol(readFile), 8)
  expect_equal(nrow(readFile), 29)
})

test_that("reading files working txt", {
  readFile<-rrcData(testthat::test_path("dataForTests", "DP.CA2.txt"),
                    s = ",", d = ".", colsPed = 1, colsTraits = 4:8)
  expect_equal(ncol(readFile), 8)
  expect_equal(nrow(readFile), 29)
})

test_that("reading files working xls", {
  readFile<-rrcData(testthat::test_path("dataForTests", "SDP.SCA2.xls"),
                    s = ",", d = ".", colsPed = 1, colsTraits = 4:8)
  expect_equal(ncol(readFile), 8)
  expect_equal(nrow(readFile), 39)
})

test_that("reading files working xlsx", {
  readFile<-rrcData(testthat::test_path("dataForTests", "DP.SCA2.xlsx"),
                    s = ",", d = ".", colsPed = 1, colsTraits = 4:8)
  expect_equal(ncol(readFile), 8)
  expect_equal(nrow(readFile), 29)
})

test_that("reading files working Sem extensão", {
  readFile<-rrcData(testthat::test_path("dataForTests", "SDPCA2"),
                    s = ",", d = ".", colsPed = 1, colsTraits = 4:8)
  expect_equal(ncol(readFile), 8)
  expect_equal(nrow(readFile), 39)
})

test_that("reading files working ODS", {
  readFile<-rrcData(testthat::test_path("dataForTests", "TesteODS.ods"),
                    s = ",", d = ".", colsPed = 1, colsTraits = 4:8)
  expect_equal(ncol(readFile), 8)
  expect_equal(nrow(readFile), 29)
})

test_that("recoding is working", {
  unrecodedData<-read.csv(testthat::test_path("dataForTests", "DP-CA.csv"),
                    s = ",", d = ".")
  recodedData<-rrcData(testthat::test_path("dataForTests", "DP-CA.csv"),
                       s = ",", d = ".", colsPed = 1, colsTraits = 4:8)

  expect_equal(length(unrecodedData), length(recodedData))
  expect_equal(recodedData[,2], unrecodedData[,2])
  expect_equal(nlevels(as.factor(recodedData[,3])), nlevels(as.factor(unrecodedData[,3])))
})
