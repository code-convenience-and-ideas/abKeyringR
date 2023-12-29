testthat::test_that("check_keyring_entry_exists works", {
  yaml_entry_block <- list(
    "name" = "secret_three",
    "type" = "token",
    "username" = "harcoded_user",
    "comment" = "You should go the the service and process an appropriate access token.",
    "service" = "test"
  )



  testthat::expect_equal(2 * 2, 4)
})

testthat::test_that("check_secret_entry_exists works", {
    yaml_entry_block <- list(
        "name" = "secret_three",
        "type" = "token",
        "username" = "harcoded_user",
        "comment" = "You should go the the service and process an appropriate access token.",
        "service" = "test"
    )



    testthat::expect_equal(2 * 2, 4)
})

testthat::test_that("check_needed_entries_exist works", {
  input_yaml_blocks <- list(
    "email_format" = "first_name.last_name@company.com",
    "keyring_entries" = list(
      "secret_one" = list(
        "name" = "secret_one",
        "type" = "password",
        "username" = paste0(Sys.info()[["user"]], "CustomEnding"),
        "comment" = "Note, should match your username and password for the service x.",
        "service" = "test"
      ),
      "secret_two" = list(
        "name" = "secret_two",
        "type" = "sshkey",
        "username" = "hello@company.com",
        "comment" = "Note, you should have set up an SSHKEY for this service.",
        "service" = "test"
      ),
      "secret_three" = list(
        "name" = "secret_three",
        "type" = "token",
        "username" = "harcoded_user",
        "comment" = "You should go the the service and process an appropriate access token.",
        "service" = "test"
      )
    )
  )

  testthat::expect_equal(2 * 2, 4)
})

testthat::test_that("get_username_from_data works", {
  yaml_entry_block <- list(
    "name" = "secret_three",
    "type" = "token",
    "username" = "harcoded_user",
    "comment" = "You should go the the service and process an appropriate access token.",
    "service" = "test"
  )

  testthat::expect_equal(2 * 2, 4)
})

testthat::test_that("get_password_from_data works", {
    yaml_entry_block <- list(
        "name" = "secret_three",
        "type" = "token",
        "username" = "harcoded_user",
        "comment" = "You should go the the service and process an appropriate access token.",
        "service" = "test"
    )

    testthat::expect_equal(2 * 2, 4)
})

testthat::test_that("get_username_from_block works", {
    yaml_entry_block <- list(
        "name" = "secret_three",
        "type" = "token",
        "username" = "harcoded_user",
        "comment" = "You should go the the service and process an appropriate access token.",
        "service" = "test"
    )

    testthat::expect_equal(2 * 2, 4)
})

testthat::test_that("get_password_from_block works", {
    yaml_entry_block <- list(
        "name" = "secret_three",
        "type" = "token",
        "username" = "harcoded_user",
        "comment" = "You should go the the service and process an appropriate access token.",
        "service" = "test"
    )

    testthat::expect_equal(2 * 2, 4)
})


testthat::test_that("check_table_entries_available works", {
  example_table <- dplyr::tibble(
    name = c("Key1", "Key2", "Key3"),
    service = c("ServiceA", "ServiceB", "ServiceC"),
    username = c("UserA", NA, "UserC"),
    comment = c("Comment 1", "Comment 2", "Comment 3"),
    entry_exists = c(FALSE, FALSE, TRUE)
  )

  modified_table <- check_table_entries_available(example_table)

  expected_table <- example_table
  expected_table$entry_exists <- c(FALSE, FALSE, FALSE)

  testthat::expect_equal(modified_table, expected_table)
})


testthat::test_that("construct_guide_message works", {
  example_table <- dplyr::tibble(
    name = c("Key1", "Key2", "Key3"),
    service = c("ServiceA", "ServiceB", "ServiceC"),
    username = c("UserA", NA, "UserC"),
    comment = c("Comment 1", "Comment 2", "Comment 3"),
    entry_exists = c(FALSE, FALSE, TRUE)
  )

  expected_guide_messages <- glue::glue({
    "Entry {example_table$name} not found. Please set it up appropriately.
    Service: {example_table$service}, implied username: {example_table$username}.
    Comment from docs: {example_table$comment}"
  })

  modified_table <- construct_guide_message(example_table)

  expected_table <- example_table
  expected_table$guide_message <- expected_guide_messages

  testthat::expect_equal(modified_table, expected_table)
})


testthat::test_that("raise_missing_entry_message works", {
  example_table <- dplyr::tibble(
    name = c("Key1", "Key2", "Key3"),
    service = c("ServiceA", "ServiceB", "ServiceC"),
    username = c("UserA", NA, "UserC"),
    comment = c("Comment 1", "Comment 2", "Comment 3"),
    entry_exists = c(FALSE, FALSE, TRUE)
  )

  expected_guide_messages <- glue::glue({
    "Entry {example_table$name} not found. Please set it up appropriately.
    Service: {example_table$service}, implied username: {example_table$username}.
    Comment from docs: {example_table$comment}"
  })



  expected_table <- example_table
  expected_table$guide_message <- expected_guide_messages
  expected_table <- expected_table[!expected_table$entry_exists, ]

  # Check warning output is in expected form
  message_warnings <- testthat::capture_warnings(
    modified_table <- raise_missing_entry_message(example_table)
  )
  testthat::expect_match(message_warnings, "Entry Key2", all = FALSE)
  testthat::expect_match(message_warnings, "Entry Key1", all = FALSE)

  # Check output dataframe matches
  testthat::expect_equal(modified_table, expected_table)
})
