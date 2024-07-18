library(testthat)
library(sf)

create_dummy_dataset <- function(crs) {
	point <- st_sfc(st_point(c(0, 0)), crs = crs)
	sf_data <- st_sf(geometry = point)
	return(sf_data)
}

test_that("CRS code is correctly assigned to and retreived from dummy dataset", {
	expected_crs <- 'EPSG:4326'
	dummy_dataset <- create_dummy_dataset(expected_crs)
	expect_equal(st_crs(dummy_dataset)$input, expected_crs)
})

test_that("CRS version 0.1.0 is correctly parsed and handled", {
	expected_crs <- 'EPSG:4326'
	crs_0_1_0 <- st_crs(expected_crs)
	dummy_dataset <- create_dummy_dataset(crs_0_1_0)
	expect_equal(st_crs(dummy_dataset)$wkt, as.character(crs_0_1_0)[2])
})

test_that("Invalid CRS version is correctly handled", {
	invalid_crs <- "INVALID_CRS"
	dummy_dataset <- create_dummy_dataset(invalid_crs)
	expect_true(is.na(st_crs(dummy_dataset)))
})

test_that("File using spec 1.2.0-dev is handled correctly", {
	tmp_file <- st_read_parquet("example-1.2.0-dev.parquet")
	expected_crs <- "OGC:CRS84"
	expect_equal(st_crs(tmp_file)$input, expected_crs)
})

test_that("File using spec 0.1.0 is handled correctly", {
	#NOT DONE
	tmp_file <- st_read_parquet("example-0.1.0.parquet")
	expected_crs <- "North_America_Albers_Equal_Area_Conic"
	expect_equal(st_crs(tmp_file)$input, expected_crs)
})

test_that("Files saved by sfarrow are correctly re-loaded", {
	
	expected_crs <- 'ESRI:102008'
	dummy_dataset <- create_dummy_dataset(expected_crs)
	suppressWarnings(st_write_parquet(dummy_dataset,"dummy.parquet"))
	tmp_file <- st_read_parquet("dummy.parquet")
	expect_equal(st_crs(tmp_file)$input, "North_America_Albers_Equal_Area_Conic")
})



