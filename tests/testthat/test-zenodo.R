library(testthat)

# use a real, public concept DOI from the Zenodo sandbox for testing
# this record has multiple versions
test_doi <- "10.5072/zenodo.308201"

test_that("resolve_zenodo_version works with a live sandbox request", {
  skip_on_cran()
  skip_on_ci()
  # test resolving the latest version
  latest_version <- resolve_zenodo_version(test_doi, version = "latest", sandbox = TRUE, max_attempts = 15)
  expect_equal(latest_version, "1.0.2")
  
  # test resolving a specific, existing version
  specific_version <- resolve_zenodo_version(test_doi, version = "1.0.0", sandbox = TRUE, max_attempts = 15)
  expect_equal(specific_version, "1.0.0")
  
  # test error for a non-existent version
  expect_error(
    resolve_zenodo_version(test_doi, version = "v9.9.9", sandbox = TRUE, max_attempts = 15),
    "Version 9.9.9 not found"
  )
})

test_that("download_from_zenodo constructs the correct URL and returns dest_path", {
  skip_on_cran()
  skip_on_ci()

  temp_dest <- tempfile(fileext = ".tsv")
  captured_url <- NULL

  testthat::local_mocked_bindings(
    download_with_progress = function(url, dest_path) {
      captured_url <<- url
      file.create(dest_path)
      invisible(dest_path)
    },
    .package = "openesm"
  )

  result_path <- download_from_zenodo(
    version_doi = "10.5072/zenodo.308201",
    dataset_id = "0001",
    author_name = "test",
    sandbox = TRUE,
    dest_path = temp_dest
  )

  expect_equal(result_path, temp_dest)
  expect_equal(
    captured_url,
    "https://sandbox.zenodo.org/records/308201/files/0001_test_ts.tsv"
  )

  unlink(temp_dest)
})

