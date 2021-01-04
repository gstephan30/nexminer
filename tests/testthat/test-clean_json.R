test_that("item import works", {
  library(nexminer)
  bl_raw <- import_item("black lotus", "patchwerk", "horde")
  clean_json(bl_raw)
})
