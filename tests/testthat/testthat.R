context("polygon_ds")
test_that("classification of scarse data",
          {
            data("World",package = "tmap")
            World <- World[(!is.na(World$life_exp)),]
            polygon_division <- polygon_ds(World,
                                           data=9:10,
                                           k=5,
                                           style="B",
                                           disjoint = TRUE)
            expect_equal(
              head(polygon_division),
              c(1, 3, 4, 1, 1, 3))
          })
context("points_ds")
test_that("classification of point data",
          {
            data("quakes")
            quakes <- SpatialPointsDataFrame(
              cbind(
                quakes$lat, quakes$long),
              quakes)
            point_division <- points_ds(
              quakes,
              data=3:4,
              k=5,
              style="B")
            expect_equal(
              head(point_division),
              c(4, 4, 5, 1, 4, 1))
          })
