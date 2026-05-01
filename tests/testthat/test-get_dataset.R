library(testthat)

test_that("get_dataset downloads and processes a single dataset", {
  skip_on_cran()
  skip_on_ci()
  
  # create a temp file that actually exists
  temp_data_file <- tempfile(fileext = ".tsv")
  readr::write_tsv(data.frame(x = 1:5), temp_data_file)
  
  testthat::local_mocked_bindings(
    list_datasets = function(cache_hours = 24, metadata_version = "latest", max_attempts = 15) {
      json_string <- create_mock_dataset_json()
      raw_list <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
      openesm:::process_raw_datasets_list(raw_list)
    },
    resolve_zenodo_version = function(doi, version, sandbox, max_attempts = 15) "1.0.0",
    get_zenodo_versions = function(zenodo_doi, sandbox = FALSE) {
      data.frame(
        id = "1234567", version = "1.0.0",
        doi = "10.5281/zenodo.1234567", date = "2020-01-01",
        stringsAsFactors = FALSE
      )
    },
    download_from_zenodo = function(version_doi, dataset_id, author_name, sandbox = FALSE, dest_path = NULL) {
      file.copy(temp_data_file, dest_path)
      return(dest_path)
    },
    get_cache_path = function(...) temp_data_file,
    read_json_safe = function(path) {
      list(
        dataset_id = "0001",
        first_author = "smith",
        year = 2020,
        zenodo_doi = "10.5281/zenodo.1234567",
        license = "CC-BY-4.0"
      )
    },
    .package = "openesm"
  )
  
  testthat::local_mocked_bindings(
    file_exists = function(path) TRUE,
    .package = "fs"
  )
  
  dataset <- suppressMessages(capture.output(
    ds <- get_dataset("0001")
  ))
  
  expect_s3_class(ds, "openesm_dataset")
  expect_equal(ds$dataset_id, "0001")
  expect_equal(ds$dataset_version, "1.0.0")
  expect_equal(nrow(ds$data), 5)
  
  # cleanup
  unlink(temp_data_file)
})

test_that("get_multiple_datasets works correctly", {
  skip_on_cran()
  skip_on_ci()
  
  testthat::local_mocked_bindings(
    get_multiple_datasets = function(dataset_ids, ...) {
      datasets <- lapply(dataset_ids, function(id) {
        structure(list(dataset_id = id), class = "openesm_dataset")
      })
      names(datasets) <- dataset_ids
      structure(datasets, class = c("openesm_dataset_list", "list"))
    }
  )
  
  datasets <- get_dataset(c("ds1", "ds2"))

  expect_s3_class(datasets, "openesm_dataset_list")
  expect_length(datasets, 2)
  expect_equal(names(datasets), c("ds1", "ds2"))
})

test_that("get_multiple_datasets can be called explicitly", {
  skip_on_cran()
  skip_on_ci()
  
  testthat::local_mocked_bindings(
    get_dataset = function(dataset_id, ...) {
      structure(
        list(dataset_id = dataset_id, dataset_version = "1.0.0", metadata_version = "1.0.0"),
        class = "openesm_dataset"
      )
    }
  )
  
  datasets <- get_multiple_datasets(c("ds1", "ds2"), version = "latest")
  
  expect_s3_class(datasets, "openesm_dataset_list")
  expect_length(datasets, 2)
  expect_equal(names(datasets), c("ds1", "ds2"))
})

test_that("get_dataset errors for non-existent dataset", {
  skip_on_cran()
  skip_on_ci()
  
  testthat::local_mocked_bindings(
    list_datasets = function(cache_hours = 24, metadata_version = "latest", max_attempts = 15) {
      json_string <- create_mock_dataset_json()
      raw_list <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
      openesm:::process_raw_datasets_list(raw_list)
    },
    resolve_zenodo_version = function(doi, version, sandbox, max_attempts = 15) "1.0.0",
    .package = "openesm"
  )
  
  expect_error(get_dataset("9999"), "Dataset with id \"9999\" not found")
})

