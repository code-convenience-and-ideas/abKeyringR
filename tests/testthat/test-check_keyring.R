testthat::test_that("check_keyring_entry_exists works", {
  # Add a temporary keyring entry for testing
  keyring::key_set_with_value("database", "username", "password123")

  # Check true and false scenario
  testthat::expect_equal(
    check_keyring_entry_exists('database', 'username'), TRUE
  )

  testthat::expect_equal(
    check_keyring_entry_exists('databasexytas', 'username'), FALSE
  )

  # Clean up: remove the temporary keyring entry
  keyring::key_delete("database", "username")
})

testthat::test_that("check_secret_entry_exists works", {
  temp_username <- "my_user"
  temp_password <- "my_pswd"

  # Add a temporary keyring entry for testing
  keyring::key_set_with_value("database", "username", temp_username)
  keyring::key_set_with_value("database", temp_username, temp_password)

  # Check value
  keyring_block_list <- list("service" = "database", "username" = temp_username)
  keyring_block_list_garbage <- list("service" = "asdwadsda", "username" = "qawdasd")
  keyring_block_service_nonuser <- list("service" = "database", "username" = "qawdasd")
  keyring_value_true <- check_secret_entry_exists(keyring_block_list)
  keyring_value_false <- check_secret_entry_exists(keyring_block_list_garbage)
  keyring_value_nonuser <- check_secret_entry_exists(keyring_block_service_nonuser)

  testthat::expect_equal(
    keyring_value_true, TRUE
  )

  testthat::expect_equal(
    keyring_value_false, FALSE
  )

  testthat::expect_equal(
    keyring_value_nonuser, FALSE
  )

  # clean up: remove temp keyring entry
  keyring::key_delete("database", "username")
  keyring::key_delete("database", temp_username)

})

testthat::test_that("check_needed_entries_exist works", {
  temp_username <- "my_user"
  temp_password <- "my_pswd"

  # Add a temporary keyring entry for testing
  keyring::key_set_with_value("database", "username", temp_username)
  keyring::key_set_with_value("database", temp_username, temp_password)


  keyring_block_list <- list("service" = "database", "username" = temp_username)
  keyring_block_list_garbage <- list("service" = "asdwadsda", "username" = "qawdasd")
  keyring_block_service_nonuser <- list("service" = "database", "username" = "qawdasd")

  keyring_data_format <- list("email_format" = "",
                              "keyring_entries" =
                                list("secret_one" = keyring_block_list,
                                     "secret_two" = keyring_block_list_garbage,
                                     "secret_three" = keyring_block_service_nonuser))

  output_keyring_list <- check_needed_entries_exist(keyring_data_format)

  expected_presence_list <- list("secret_one" = TRUE,
                                 "secret_two" = FALSE,
                                 "secret_three" = FALSE)

  testthat::expect_equal(output_keyring_list,
                         expected_presence_list)

  # clean up: remove temp keyring entry
  keyring::key_delete("database", "username")
  keyring::key_delete("database", temp_username)
})


testthat::test_that("get_username_from_block works", {
  temp_username <- "my_user"
  temp_password <- "my_pswd"

  # Add a temporary keyring entry for testing
  keyring::key_set_with_value("database", "username", temp_username)
  keyring::key_set_with_value("database", temp_username, temp_password)


  keyring_block_list_one <- list("service" = "database",
                                 "username" = temp_username)

  username_from_keyring <- get_username_from_block(keyring_block_list_one)

  testthat::expect_equal(username_from_keyring, temp_username)

  # clean up: remove temp keyring entry
  keyring::key_delete("database", "username")
  keyring::key_delete("database", temp_username)
})

testthat::test_that("get_password_from_block works", {
  temp_username <- "my_user"
  temp_password <- "my_pswd"

  # Add a temporary keyring entry for testing
  keyring::key_set_with_value("database", "username", temp_username)
  keyring::key_set_with_value("database", temp_username, temp_password)


  keyring_block_list_one <- list("service" = "database",
                                 "username" = temp_username)

  password_from_keyring <- get_password_from_block(keyring_block_list_one)

  testthat::expect_equal(password_from_keyring, temp_password)

  # clean up: remove temp keyring entry
  keyring::key_delete("database", "username")
  keyring::key_delete("database", temp_username)
})


testthat::test_that("get_username_from_data works", {
  temp_username <- "my_user"
  temp_password <- "my_pswd"

  # Add a temporary keyring entry for testing
  keyring::key_set_with_value("database", "username", temp_username)
  keyring::key_set_with_value("database", temp_username, temp_password)


  keyring_block_list <- list("service" = "database", "username" = temp_username)
  keyring_block_list_garbage <- list("service" = "asdwadsda", "username" = "qawdasd")
  keyring_block_service_nonuser <- list("service" = "database", "username" = "qawdasd")

  keyring_data_format <- list(
    "email_format" = "",
    "keyring_entries" =
      list("secret_one" = keyring_block_list,
           "secret_two" = keyring_block_list_garbage,
           "secret_three" = keyring_block_service_nonuser))


  username_from_keyring <- get_username_from_data(keyring_data_format,
                                                  "secret_one")

  testthat::expect_equal(username_from_keyring, temp_username)

  # clean up: remove temp keyring entry
  keyring::key_delete("database", "username")
  keyring::key_delete("database", temp_username)
})

testthat::test_that("get_password_from_data works", {
  temp_username <- "my_user"
  temp_password <- "my_pswd"

  # Add a temporary keyring entry for testing
  keyring::key_set_with_value("database", "username", temp_username)
  keyring::key_set_with_value("database", temp_username, temp_password)


  keyring_block_list <- list("service" = "database", "username" = temp_username)
  keyring_block_list_garbage <- list("service" = "asdwadsda", "username" = "qawdasd")
  keyring_block_service_nonuser <- list("service" = "database", "username" = "qawdasd")

  keyring_data_format <- list(
    "email_format" = "",
    "keyring_entries" =
      list("secret_one" = keyring_block_list,
           "secret_two" = keyring_block_list_garbage,
           "secret_three" = keyring_block_service_nonuser))

  password_from_keyring <- get_password_from_data(keyring_data_format,
                                                  "secret_one")

  testthat::expect_equal(password_from_keyring, temp_password)

  # clean up: remove temp keyring entry
  keyring::key_delete("database", "username")
  keyring::key_delete("database", temp_username)
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
