#test-formatPed

test_that("Pedigree com caracteres não ASCII ", {
  expect_error(
    check_ped_ascii(ped_ascii_bad())
  )
})
