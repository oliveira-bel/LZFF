
test_that("testando se o nome possui  #", {
  expect_error(
    formatA(testData, of = "filetest.txt#"),
    "o nome do arquivo não pode possuir #"
  )
})
