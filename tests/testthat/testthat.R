context("ds_polygon")
test_that("regionalization of polygon data (ds_polygon)",
          {
            data("socioGrid")
            socioGrid$class <- ds_polygon(socioGrid, k = 7,
               disjoint = TRUE, plot = TRUE, explain = FALSE)
            
            expect_equal(
              head(socioGrid$class),
              c(6, 5, 3, 1, 2, 6))
          })
context("ds_points")
test_that("regionalization of point data (ds_points)",
          {
            data("realEstate")
            realEstate$class <- ds_points(realEstate, k = 5, explain = FALSE)
            expect_equal(
              head(realEstate$class),
              c(1, 5, 4, 2, 4, 4))
          })

context("regionalize")
test_that("regionalization of point data (regionalize)",
          {
            data("realEstate")
            realEstate$class <- regionalize(realEstate, k = 5, explain = FALSE)
            expect_equal(
              head(realEstate$class),
              c(1, 5, 4, 2, 4, 4))
          })
test_that("regionalization of polygon data (regionalize)",
          {
            data("socioGrid")
            socioGrid$class <- regionalize(socioGrid, k = 7,
                                          disjoint = TRUE, plot = TRUE,
                                          explain = FALSE)
            
            expect_equal(
              head(socioGrid$class),
              c(6, 5, 3, 1, 2, 6))
          })

#Need tests for accuracy