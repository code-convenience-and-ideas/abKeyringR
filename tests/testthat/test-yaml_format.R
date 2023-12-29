testthat::test_that("get_entry_names_from_field works", {
  # extract values in simple case
  example_list <- list(
    list("name" = "a"),
    list("name" = "b")
  )

  testthat::expect_equal(get_entry_names_from_field(example_list), c("a", "b"))

  # Fails when entries don't exist
})

testthat::test_that("promote_keyring_entry_names works", {
  test_yaml <- list(
    "email_format" = "first_name.last_name@company.com",
    "keyring_entries" = list(
      list("name" = "keyring_one"),
      list("name" = "keyring_two")
    )
  )

  keyring_named_yaml <- list(
    "email_format" = "first_name.last_name@company.com",
    "keyring_entries" = list(
      "keyring_one" = list("name" = "keyring_one"),
      "keyring_two" = list("name" = "keyring_two")
    )
  )

  processed_keyring_yaml <- promote_keyring_entry_names(
    test_yaml
  )

  # Will raise error until implementing proper unit test
  testthat::expect_equal(processed_keyring_yaml, keyring_named_yaml)
})

testthat::test_that("update_list_of_keyrings works", {
  shared_email_template <- "first_name.last_name@company.com"
  test_yaml <- list(
    "email_format" = shared_email_template,
    "keyring_entries" = list(
      "keyring_one" = list(
        "name" = "keyring_one",
        "username" = "hardcoded"
      ),
      "keyring_two" = list(
        "name" = "keyring_two",
        "username" = "{system_user}"
      ),
      "keyring_three" = list(
        "name" = "keyring_three",
        "username" = "{system_email}"
      )
    )
  )

  keyring_named_yaml <- list(
    "email_format" = shared_email_template,
    "keyring_entries" = list(
      "keyring_one" = list(
        "name" = "keyring_one",
        "username" = "hardcoded"
      ),
      "keyring_two" = list(
        "name" = "keyring_two",
        "username" = Sys.info()[["user"]]
      ),
      "keyring_three" = list(
        "name" = "keyring_three",
        "username" = "hello@company.com"
      )
    )
  )

  email_processor <- function(x) {
    "hello"
  }

  processed_keyring_yaml <- update_list_of_keyrings(
    test_yaml,
    shared_email_template,
    email_processor
  )

  # Will raise error until implementing proper unit test
  testthat::expect_equal(processed_keyring_yaml, keyring_named_yaml)
})

testthat::test_that("update_keyring_block works", {
  simple_keyring_block <- list(
    "name" = "keyring_one", "username" = "hardcoded"
  )

  username_keyring_block <- list(
    "name" = "keyring_one", "username" = "{system_user}"
  )

  useremail_keyring_block <- list(
    "name" = "keyring_one", "username" = "{system_email}"
  )

  email_format <- "first_name.last_name@company.com"
  email_processor <- function(x) {
    "hello"
  }

  processed_simple_block <- update_keyring_block(
    simple_keyring_block, email_format, email_processor
  )

  processed_username_block <- update_keyring_block(
    username_keyring_block, email_format, email_processor
  )

  processed_useremail_block <- update_keyring_block(
    useremail_keyring_block, email_format, email_processor
  )

  # expectations
  expected_username_block <- processed_username_block
  expected_username_block$username <- Sys.info()[["user"]]

  expected_useremail_block <- processed_useremail_block
  expected_useremail_block$username <- "hello@company.com"

  # Will raise error until implementing proper unit test
  testthat::expect_equal(
    processed_simple_block,
    simple_keyring_block
  )

  testthat::expect_equal(
    processed_username_block,
    expected_username_block
  )

  testthat::expect_equal(
    processed_useremail_block,
    processed_useremail_block
  )
})

testthat::test_that("load_keyring_yaml works", {
  local_folder_yaml_path <- system.file(
    "extdata", "example_keyring_entries.yaml",
    package = "abKeyringR"
  )

  expected_yaml_output <- list(
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

  email_processor <- function(x) {
    return("hello")
  }

  processed_keyring_yaml <- load_keyring_yaml(
    local_folder_yaml_path,
    email_processor
  )

  # Will raise error until implementing proper unit test
  testthat::expect_equal(processed_keyring_yaml, expected_yaml_output)
})

testthat::test_that("", {
  yaml_data_example <- list("keyring_entries" = list(
    list("name" = "test_one", my_row_data = 1),
    list("name" = "test_two", my_row_data = 2),
    list("name" = "test_three", my_row_data = 3)
  ))
  table_of_yaml_data <- keyring_yaml_to_df(yaml_data_example)

  expected_table_output <- dplyr::tribble(
    ~name, ~my_row_data,
    "test_one", 1,
    "test_two", 2,
    "test_three", 3
  )

  testthat::expect_equal(table_of_yaml_data, expected_table_output)
})
