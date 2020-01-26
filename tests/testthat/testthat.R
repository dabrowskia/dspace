context("polygon_ds")
test_that("classification of scarse data",
          {
            data("socioGrid")
            socioGrid$class <- polygon_ds(socioGrid, k = 7,
               disjoint = TRUE, plot = TRUE, accuracy = FALSE)
            
            expect_equal(
              head(socioGrid$class),
              c(6, 5, 3, 1, 2, 6))
          })
context("points_ds")
test_that("classification of point data",
          {
            data("realEstate")
            realEstate$class <- points_ds(realEstate, k = 5, accuracy = FALSE)
            expect_equal(
              head(realEstate$class),
              c(1, 5, 4, 2, 4, 4))
          })
