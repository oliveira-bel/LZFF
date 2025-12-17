#test-formatPed

test_that("Pedigree com caracteres ASCII passa no teste", {

  pedObj <- helper_ped_ascii()

  expect_true(check_ped_ascii(pedObj))
})
