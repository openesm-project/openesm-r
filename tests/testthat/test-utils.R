library(testthat)

test_that("get_cache_path constructs paths and creates directories", {
  temp_dir <- tempfile("cache")
  testthat::local_mocked_bindings(
    get_metadata_dir = function() fs::path(temp_dir, "metadata"),
    get_data_dir = function() fs::path(temp_dir, "data"),
    .package = "openesm"
  )
  
  # test metadata path
  meta_path <- get_cache_path("0001", "v1", "meta.json", type = "metadata")
  expected_meta_path <- fs::path(temp_dir, "metadata", "0001", "v1", "meta.json")
  expect_equal(meta_path, expected_meta_path)
  expect_true(fs::dir_exists(fs::path_dir(meta_path)))
  
  # test data path
  data_path <- get_cache_path("0002", "v2", "data.tsv", type = "data")
  expected_data_path <- fs::path(temp_dir, "data", "0002", "v2", "data.tsv")
  expect_equal(data_path, expected_data_path)
  expect_true(fs::dir_exists(fs::path_dir(data_path)))
  
  unlink(temp_dir, recursive = TRUE)
})


test_that("get_cache_dir returns correct paths", {
  skip_on_cran()
  skip_on_ci()
  temp_dir <- tempfile("cache")
  dir.create(temp_dir)
  
  testthat::local_mocked_bindings(
    get_cache_dir = function(type) {
      if (type == "metadata") {
        return(fs::path(temp_dir, "metadata"))
      } else if (type == "data") {
        return(fs::path(temp_dir, "data"))
      } else {
        stop("Unknown cache type")
      }
    }
  )
  meta_path <- get_cache_dir(type = "metadata")
  data_path <- get_cache_dir(type = "data")
  expect_equal(meta_path, fs::path(temp_dir, "metadata"))
  expect_equal(data_path, fs::path(temp_dir, "data"))
  expect_equal(get_metadata_dir(), fs::path(temp_dir, "metadata"))
  expect_equal(get_data_dir(), fs::path(temp_dir, "data"))
  
  unlink(temp_dir, recursive = TRUE)
})


test_that("cache_info works when cache exists", {
  # Mock get_cache_dir to point to a controlled temporary directory
  temp_dir <- tempfile("testcache")
  dir.create(temp_dir)
  # Create a dummy file to give the cache some size
  writeLines("some data", file.path(temp_dir, "dummy.txt"))
  
  testthat::local_mocked_bindings(
    get_cache_dir = function() temp_dir
  )
  
  # Capture the output and check for expected messages
  output <- cli::cli_fmt(cache_info())
  expect_true(any(grepl("Cache location:", output)))
  expect_true(any(grepl("Cache size:", output)))
  
  unlink(temp_dir, recursive = TRUE)
})

test_that("cache_info works when cache does not exist", {
  temp_dir <- tempfile("nonexistent")
  testthat::local_mocked_bindings(
    get_cache_dir = function() temp_dir
  )
  
  # Capture the output and check for the specific message
  output <- cli::cli_fmt(cache_info())
  expect_true(any(grepl("Cache directory does not exist yet", output)))
})

test_that("clear_cache works non-interactively with force = TRUE", {
  temp_dir <- tempfile("testcache")
  dir.create(temp_dir)
  writeLines("test", file.path(temp_dir, "file.txt"))
  
  testthat::local_mocked_bindings(
    get_cache_dir = function() temp_dir
  )
  
  expect_true(fs::dir_exists(temp_dir))
  
  # Capture output and check for success message
  output <- cli::cli_fmt(clear_cache(force = TRUE))
  expect_true(any(grepl("Cache cleared", output)))
  
  expect_false(fs::dir_exists(temp_dir))
})

test_that("clear_cache handles non-existent directory gracefully", {
  testthat::local_mocked_bindings(
    get_cache_dir = function() tempfile("nonexistent")
  )
  
  # Capture output and check for the correct informational message
  output <- cli::cli_fmt(clear_cache(force = TRUE))
  expect_true(any(grepl("Cache directory does not exist", output)))
})

test_that("read_json_safe handles errors correctly", {
  # test with a non-existent file, which should trigger an error
  expect_error(
    read_json_safe("a_file_that_does_not_exist.json"),
    "Failed to read JSON file"
  )
  
  # test with a malformed file
  malformed_file <- tempfile()
  # write invalid json (single quotes instead of double)
  writeLines("{'key': 'value'}", malformed_file)
  expect_error(
    read_json_safe(malformed_file),
    "Failed to read JSON file"
  )
  unlink(malformed_file)
})

test_that("get_cache_dir returns correct path without type parameter", {
  # test basic functionality
  cache_dir <- get_cache_dir()
  expect_type(cache_dir, "character")
  
  expect_true(grepl("openesm", cache_dir))
  
  # directory should exist after calling the function
  expect_true(fs::dir_exists(cache_dir))
})

test_that("get_cache_dir returns correct path with type parameter", {
  # test with a type parameter
  cache_dir <- get_cache_dir(type = "datasets")
  
  # should be a character string
  expect_type(cache_dir, "character")
  
  # should contain both "openesm" and "datasets" in the path
  expect_true(grepl("openesm", cache_dir))
  expect_true(grepl("datasets", cache_dir))
  
  # directory should exist after calling the function
  expect_true(fs::dir_exists(cache_dir))
  
  # test with another type
  cache_dir_meta <- get_cache_dir(type = "metadata")
  expect_true(grepl("metadata", cache_dir_meta))
  expect_true(fs::dir_exists(cache_dir_meta))
})

test_that("get_cache_dir creates nested directories correctly", {
  # test with nested path
  cache_dir <- get_cache_dir(type = "deep/nested/path")
  
  # should contain the nested structure
  expect_true(grepl("deep", cache_dir))
  expect_true(grepl("nested", cache_dir))
  expect_true(grepl("path", cache_dir))
  
  # directory should exist
  expect_true(fs::dir_exists(cache_dir))
})


test_that("process_specific_metadata handles complete metadata correctly", {
  # Create a mock raw list similar to what jsonlite produces
  raw_meta <- list(
    dataset_id = "0001",
    first_author = "Smith",
    year = 2020,
    n_participants = 50,
    topics = list("anxiety", "depression"), # List that needs collapsing
    features = list(
      list(name = "mood", type = "integer"),
      list(name = "context", type = "string")
    )
  )

  # Call the internal function
  result <- openesm:::process_specific_metadata(raw_meta)

  # Check basic fields
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$dataset_id, "0001")
  expect_equal(result$first_author, "Smith")
  
  # Check type conversion (year and n_participants)
  expect_type(result$year, "double")
  expect_equal(result$year, 2020L)
  expect_type(result$n_participants, "double")
  expect_equal(result$n_participants, 50L)

  # Check list collapsing logic
  expect_equal(result$topics, "anxiety, depression")

  # Check nested features tibble
  expect_type(result$features, "list")
  expect_s3_class(result$features[[1]], "tbl_df")
  expect_equal(nrow(result$features[[1]]), 2)
})

test_that("process_specific_metadata handles missing or NULL values", {
  # Minimal input
  raw_meta <- list(
    dataset_id = "0002"
    # Missing other fields
  )

  result <- openesm:::process_specific_metadata(raw_meta)

  expect_equal(result$dataset_id, "0002")
  
  # Check that missing character fields are NA_character_
  expect_true(is.na(result$first_author))
  expect_type(result$first_author, "character")

  # Check that missing integer fields are NA_integer_
  expect_true(is.na(result$n_participants))
  expect_type(result$n_participants, "integer")
  
  # Check empty features
  expect_equal(nrow(result$features[[1]]), 0)
})

test_that("read_json_safe throws informative error on invalid JSON", {
  # Create a dummy file with bad JSON
  tmp <- tempfile()
  writeLines("{ bad_json: ", tmp)
  
  expect_error(
    openesm:::read_json_safe(tmp),
    "Failed to read JSON file"
  )
  
  unlink(tmp)
})

test_that("read_json_safe throws error on non-existent file", {
  expect_error(
    openesm:::read_json_safe("non_existent_file.json"),
    "Failed to read JSON file"
  )
})


