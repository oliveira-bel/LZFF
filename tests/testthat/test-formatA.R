dataList_ok <- list(data = data.frame(a = c(1, NA),b = c(3, 4)))

testthat::test_that("formatA gera erro quando o nome do arquivo contém #", {
  expect_error(formatA(dataList_ok, of = "arquivo#invalido.txt"),
    "File name cannot contain a #"
  )
})
