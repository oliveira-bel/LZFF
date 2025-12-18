#test-rrcData
test_that("Recoding funciona corretamente nas colunas ", {

  h <- helper_recode_data()

  resultado <- recode_dados(
    dados    = h$dados,
    colsPdg  = h$colsPdg,
    colsTrts = h$colsTrts
  )

  # colunas não recodificadas
  expect_identical(resultado$id, h$dados$id)
  expect_identical(resultado$sire, h$dados$sire)
  expect_identical(resultado$trait, h$dados$trait)

})

