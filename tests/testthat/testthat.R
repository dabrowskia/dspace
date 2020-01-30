context("polygon_ds")
test_that("regionalization of polygon data (polygon_ds)",
          {
            data("socioGrid")
            socioGrid$class <- polygon_ds(socioGrid, k = 7,
               disjoint = TRUE, plot = TRUE, accuracy = FALSE)
            
            expect_equal(
              head(socioGrid$class),
              c(6, 5, 3, 1, 2, 6))
          })
context("points_ds")
test_that("regionalization of point data 9points_ds)",
          {
            data("realEstate")
            realEstate$class <- points_ds(realEstate, k = 5, accuracy = FALSE)
            expect_equal(
              head(realEstate$class),
              c(1, 5, 4, 2, 4, 4))
          })

context("regionalize")
test_that("regionalization of point data (regionalize)",
          {
            data("realEstate")
            realEstate$class <- regionalize(realEstate, k = 5, accuracy = FALSE)
            expect_equal(
              head(realEstate$class),
              c(1, 5, 4, 2, 4, 4))
          })
test_that("regionalization of polygon data (regionalize)",
          {
            data("socioGrid")
            socioGrid$class <- polygon_ds(socioGrid, k = 7,
                                          disjoint = TRUE, plot = TRUE, accuracy = FALSE)
            
            expect_equal(
              head(socioGrid$class),
              c(6, 5, 3, 1, 2, 6))
          })