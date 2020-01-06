context("polygon_ds")
test_that("classification of scarse data",
          {
            data("World",package = "tmap")
            World<-World[(!is.na(World$life_exp)),]
            expect_equal(head(polygon_ds(World,data=9:10,k=5,style="B",disjoint = T)),c(4,1,3,4,5,4))
          })
context("points_ds")
test_that("classification of point data",
          {
            data("quakes")
            quakes<-SpatialPointsDataFrame(cbind(quakes$lat,quakes$long),quakes)
            expect_equal(head(points_ds(quakes,data=3:4,k=5,style="B")),c(1,1,5,4,3,3))
          })
