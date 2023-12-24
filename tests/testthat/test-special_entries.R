testthat::test_that("load_username works", {
  current_username <- load_username()
  system_username <- Sys.info()[["user"]]

  testthat::expect_equal(current_username, system_username)
})

testthat::test_that("convert_entry_to_uppercase works", {
  lowercase_entry_to_change <- "lower"
  expected_result_on_lowercase <- "LOWER"
  lowercase_result <- convert_entry_to_uppercase(lowercase_entry_to_change)

  uppercase_entry_for_null <- "UPPER"
  uppercase_results <- convert_entry_to_uppercase(uppercase_entry_for_null)

  testthat::expect_equal(lowercase_result, expected_result_on_lowercase)
  testthat::expect_equal(uppercase_results, uppercase_entry_for_null)
})

testthat::test_that("extract_email_domain works", {
  specified_email_format <- "first_name.last_name@my_domain.com"
  extracted_email_domain <- extract_email_domain(specified_email_format)
  expected_email_domain <- "my_domain.com"

  testthat::expect_equal(extracted_email_domain, expected_email_domain)
})

testthat::test_that("load_user_email works", {
  email_start_inference <- function(x) {
    "hello"
  }
  specified_email_format <- "first_name.last_name@my_domain.com"

  inferred_user_email <- load_user_email(
    specified_email_format,
    email_start_inference
  )
  expected_user_email <- "hello@my_domain.com"

  testthat::expect_equal(inferred_user_email, expected_user_email)
})

testthat::test_that("generic_email_inferer works", {
  specified_email_format <- "first_name.last_name@my_domain.com"
  output_infered_email_prefix <- generic_email_inferer(specified_email_format)
  system_username <- Sys.info()[["user"]]

  testthat::expect_equal(output_infered_email_prefix, system_username)
})

testthat::test_that("fill_special_entry works", {
  specified_email_format <- "first_name.last_name@my_domain.com"
  hello_email_prefix <- function(x) {
    "hello"
  }

  # Do username case
  username_special_entry <- "{system_user}_mytest"
  expected_username_output <- paste0(Sys.info()[["user"]], "_mytest")
  processed_name_special_entry <- fill_special_entry(
    username_special_entry,
    specified_email_format,
    hello_email_prefix
  )

  testthat::expect_equal(
    processed_name_special_entry,
    expected_username_output
  )

  # Do useremail case
  useremail_special_entry <- "{system_email}_mytest"
  expected_useremail_output <- paste0("hello@my_domain.com", "_mytest")
  processed_email_special_entry <- fill_special_entry(
    useremail_special_entry,
    specified_email_format,
    hello_email_prefix
  )

  testthat::expect_equal(
    processed_email_special_entry,
    expected_useremail_output
  )

  # Do generic case
  non_special_entry <- "hardcoded"
  processed_non_special_entry <- fill_special_entry(
    non_special_entry,
    specified_email_format,
    hello_email_prefix
  )

  testthat::expect_equal(processed_non_special_entry, non_special_entry)
})
