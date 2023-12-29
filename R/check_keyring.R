#' @import stringr
#' @import keyring
#' @importFrom dplyr tibble mutate filter rowwise ungroup
#' @import glue
#' @import rlang
NULL

#' Checks whether a specified entry exists in the current system keyring
#'
#' @param service_name keyring service_name entry to check
#' @param user_name keyring username entry to check
#'
#' @return Boolean indicating whether entry is in the keyring or not
#' @export
#'
#' @examples
#' check_keyring_entry_exists('database', 'username')
check_keyring_entry_exists <- function(service_name, user_name){
    tryCatch({
        is.character(keyring::key_get(service_name, user_name))
    },
    error = function(cond) {
        message("Got an error trying to get keyring entry. Will return FALSE")

        condition_error_message <- conditionMessage(cond)
        missing_element <- stringr::str_detect(condition_error_message, "Element not found.")

        if ( !missing_element ) {
            message("Doesn't seem to be error due to missing element.")
            message("Here's the original error message:")
            message(condition_error_message)
        }
        # Return value
        FALSE
    },
    finally = {
    # Nothing happens here
    }
    )
}

#' checks both username and password entry exist for the keyring entry
#'
#' @param keyring_block a list capturing the info from the keyring yaml spec
#'
#' @return boolean indicate present of complete keyring entry
#' @export
#'
#' @examples
#' keyring_block_list <- list("service" = "test", "username" = "hardcode")
#' check_secret_entry_exists(keyring_block_list)
check_secret_entry_exists <- function(keyring_block){
    keyring_username_entry_present <- check_keyring_entry_exists(
        keyring_block$service,
        "username"
    )

    keyring_password_entry_present <- check_keyring_entry_exists(
        keyring_block$service,
        keyring_block$username
    )

  entry_is_complete <-
      keyring_username_entry_present & keyring_password_entry_present

  return(entry_is_complete)
}

#' Checks every required keyring entry and whether they're defined in keyring
#'
#' @param keyring_dataset the keyring yaml data per standard spec
#'
#' @return list indicating whether the keyring entry exists or not
#' @export
#'
#' @examples
#' keyring_block_list_one <- list("service" = "test", "username" = "hardcode")
#' keyring_block_list_two <- list("service" = "trial", "username" = "example")
#' keyring_data_format <- list("email_format" = "",
#' "keyring_entries" = list("secret_one" = keyring_block_list_one,
#'                          "secret_two" = keyring_block_list_two))
#' check_needed_entries_exist(keyring_data_format)
check_needed_entries_exist <- function(keyring_dataset){

 keyring_entry_exists <- list()
 candidate_keyring_entries <- names(keyring_dataset$keyring_entries)

 for (keyring_entry_name in candidate_keyring_entries ) {
     keyring_entry_exists[[keyring_entry_name]] <- (
         check_secret_entry_exists(
             keyring_dataset$keyring_entries[[keyring_entry_name]]
          )
     )
 }

 return(keyring_entry_exists)
}

#' Retrieves the username for an entry from the keyring dataset
#'
#' @param keyring_dataset the keyring yaml data per standard spec
#' @param entry_name name of the entry to get from the keyring dataset
#'
#' @return a string with username captured in the keyring
#' @export
#'
#' @examples
#' \dontrun{
#' keyring::key_set_with_value("database", "username", "my_user")
#' keyring::key_set_with_value("database", "my_user", "my_pswd")
#' keyring::key_set_with_value("database2", "username", "my_user")
#' keyring::key_set_with_value("database2", "my_user", "my_pswd")
#'
#' keyring_block_list_one <- list("service" = "database",
#'                                "username" = "my_user")
#' keyring_block_list_two <- list("service" = "database2",
#'                                "username" = "my_user")
#' keyring_data_format <- list("email_format" = "",
#' "keyring_entries" = list("secret_one" = keyring_block_list_one,
#'                          "secret_two" = keyring_block_list_two))
#' get_username_from_data(keyring_data_format, "secret_one")
#'
#' # clean up: remove temp keyring entry
#' keyring::key_delete("database", "username")
#' keyring::key_delete("database", "my_user")
#' keyring::key_delete("database2", "username")
#' keyring::key_delete("database2", "my_user")
#' }
get_username_from_data <- function(keyring_dataset, entry_name){
    current_block <- keyring_dataset$keyring_entries[[entry_name]]
    current_username <- get_username_from_block(current_block)
    return(current_username)
}

#' Retrieves the password for an entry from the keyring dataset
#'
#' @param keyring_dataset the keyring yaml data per standard spec
#' @param entry_name name of the entry to get from the keyring dataset
#'
#' @return a string with the password captured in the keyring
#' @export
#'
#' @examples
#' \dontrun{
#' keyring::key_set_with_value("database", "username", "my_user")
#' keyring::key_set_with_value("database", "my_user", "my_pswd")
#' keyring::key_set_with_value("database2", "username", "my_user")
#' keyring::key_set_with_value("database2", "my_user", "my_pswd")
#'
#' keyring_block_list_one <- list("service" = "database",
#'                                "username" = "my_user")
#' keyring_block_list_two <- list("service" = "database2",
#'                                "username" = "my_user")
#' keyring_data_format <- list("email_format" = "",
#' "keyring_entries" = list("secret_one" = keyring_block_list_one,
#'                          "secret_two" = keyring_block_list_two))
#' get_password_from_data(keyring_data_format, "secret_one")
#'
#' # clean up: remove temp keyring entry
#' keyring::key_delete("database", "username")
#' keyring::key_delete("database", "my_user")
#' keyring::key_delete("database2", "username")
#' keyring::key_delete("database2", "my_user")
#' }
get_password_from_data <- function(keyring_dataset, entry_name){
    current_block <- keyring_dataset$keyring_entries[[entry_name]]
    current_password <- get_password_from_block(current_block)
    return(current_password)
}

#' Retrieves the username for an entry from the keyring matching current block
#'
#' @param keyring_block a list capturing the info from the keyring yaml spec
#'
#' @return a string with username captured in the keyring
#' @export
#'
#' @examples
#' \dontrun{
#' keyring::key_set_with_value("database", "username", "my_user")
#' keyring::key_set_with_value("database", "my_user", "my_pswd")
#'
#' keyring_block_list_one <- list("service" = "database",
#'                                "username" = "my_user")
#' get_username_from_block(keyring_block_list_one)
#'
#' # clean up: remove temp keyring entry
#' keyring::key_delete("database", "username")
#' keyring::key_delete("database", "my_user")
#' }
get_username_from_block <- function(keyring_block){
    return(keyring::key_get(service = keyring_block$service,
                            username = "username"))
}

#' Retrieves the password for an entry from the keyring matching current block
#'
#' @param keyring_block a list capturing the info from the keyring yaml spec
#'
#' @return a string with the password captured in the keyring
#' @export
#'
#' @examples
#' \dontrun{
#' # Add a temporary keyring entry for testing
#' keyring::key_set_with_value("database", "username", "my_user")
#' keyring::key_set_with_value("database", "my_user", "my_pswd")
#'
#' keyring_block_list_one <- list("service" = "database",
#'                                "username" = "my_user")
#' get_password_from_block(keyring_block_list_one)
#'
#' # clean up: remove temp keyring entry
#' keyring::key_delete("database", "username")
#' keyring::key_delete("database", "my_user")
#' }
get_password_from_block <- function(keyring_block){
    return(keyring::key_get(service = keyring_block$service,
                            username = keyring_block$username))
}

#' Checks all entries in a prepared entry table and whether they are available
#'
#' @param keyring_table a tibble with one entry per keyring with service, user
#'
#' @return input table with new column flagging whether entry exists
#' @export
#'
#' @examples
#' # Creating a sample keyring table
#' example_table <- dplyr::tibble(
#' name = c("Key1", "Key2", "Key3"),
#' service = c("ServiceA", "ServiceB", "ServiceC"),
#' username = c("UserA", NA, "UserC"),
#' comment = c("Comment 1", "Comment 2", "Comment 3")
#' )
#' check_table_entries_available(example_table)
check_table_entries_available <- function(keyring_table){
  # Added to appease the R CMD Check syntax overlords
  entry_exists <- NULL
  service <- NULL
  username <- NULL

  entry_existence_var <- rlang::quo(entry_exists)
  service_var <- rlang::quo(service)
  username_var <- rlang::quo(username)

  # Core logic of function
    adjusted_keyring_table <- keyring_table |>
        dplyr::rowwise() |>
        dplyr::mutate(
            entry_exists = check_keyring_entry_exists(!!service_var,
                                                      !!username_var)
        ) |>
        dplyr::ungroup()

    return(adjusted_keyring_table)
}


#' Builds an informative message to summarise missing entries and next step
#'
#' @param keyring_table a tibble with one entry per keyring with service, user
#'
#' @return input table with column for guide message indicate missing entries.
#' @export
#'
#' @examples
#' # Creating a sample keyring table
#' example_table <- dplyr::tibble(
#' name = c("Key1", "Key2", "Key3"),
#' service = c("ServiceA", "ServiceB", "ServiceC"),
#' username = c("UserA", NA, "UserC"),
#' comment = c("Comment 1", "Comment 2", "Comment 3")
#' )
#' construct_guide_message(example_table)
construct_guide_message <- function(keyring_table){
    adjusted_keyring_table <- keyring_table |>
        dplyr::mutate(
            guide_message = glue::glue(
            "Entry {name} not found. Please set it up appropriately.
            Service: {service}, implied username: {username}.
            Comment from docs: {comment}"
            )
        )

    return(adjusted_keyring_table)
}

#' Raises an informative message to suggest what needs to be done.
#'
#' @param keyring_table a tibble with one entry per keyring with service, user
#'
#' @return input table only for missing entries + guide message and warning
#' @export
#'
#' @examples
#' # Creating a sample keyring table
#' example_table <- dplyr::tibble(
#' name = c("Key1", "Key2", "Key3"),
#' service = c("ServiceA", "ServiceB", "ServiceC"),
#' username = c("UserA", NA, "UserC"),
#' comment = c("Comment 1", "Comment 2", "Comment 3"),
#' entry_exists = c(FALSE, FALSE, TRUE)
#' )
#' raise_missing_entry_message(example_table)
raise_missing_entry_message <- function(keyring_table){
  # Added to appease the R CMD Check syntax overlords
  entry_exists <- NULL
   entry_existence_var <- rlang::quo(entry_exists)

    # Add the informative message
    adjusted_keyring_table <- keyring_table |>
        dplyr::filter(!(!!entry_existence_var)) |>
        construct_guide_message()

    # For each missing entry, raise a warning and use the entry comment to
    # give context on how to supply the missing values
    for (row_ind in 1:nrow(adjusted_keyring_table)){
        warning(adjusted_keyring_table[row_ind, "guide_message"])
    }

    # Return the dataframe for missing entries
    return(adjusted_keyring_table)
}
