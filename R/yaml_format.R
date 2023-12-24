#' @import stringr
#' @import yaml
NULL

#' Loops over a list to get a vector of name entries
#'
#' @param list_to_extract_names list of entries containing a name field
#'
#' @return vector of names for each entry in the list
#' @export
#'
#' @examples
#' get_entry_names_from_field(list(
#'   list("name" = "a"),
#'   list("name" = "b")
#' ))
get_entry_names_from_field <- function(list_to_extract_names) {
  return(sapply(list_to_extract_names, function(x) x$name))
}

#' Sets entry names for keyring entry to match their name field
#'
#' @param yaml_data_as_list yaml document loaded as a nested list
#'
#' @return yaml keyring entries
#' @export
#'
#' @examples
#' test_yaml <- list(
#'   "email_format" = "first_name.last_name@company.com",
#'   "keyring_entries" = list(
#'     list("name" = "keyring_one"),
#'     list("name" = "keyring_two")
#'   )
#' )
#' promote_keyring_entry_names(test_yaml)
promote_keyring_entry_names <- function(yaml_data_as_list) {
  # Fix yaml names
  keyring_entry_names <- get_entry_names_from_field(
    yaml_data_as_list$keyring_entries
  )
  names(yaml_data_as_list$keyring_entries) <- keyring_entry_names

  return(yaml_data_as_list)
}

#' Updates the username entry within a single keyring block
#'
#' @param keyring_block a single keyring entry from the yaml data
#' @param email_template a string specifying the required email address format
#' @param email_processor function which can resolve email template
#'
#' @return
#' @export
#'
#' @examples
#' email_start_inference <- function(x) {
#'   "hello"
#' }
#' specified_email_format <- "first_name.last_name@my_domain.com"
#' keyring_example_block <- list(
#'   "name" = "keyring_one",
#'   "username" = "harcoded_user"
#' )
#' update_keyring_block(
#'   keyring_example_block, specified_email_format,
#'   email_start_inference
#' )
update_keyring_block <- function(keyring_block,
                                 email_template,
                                 email_processor) {
  # Expand and process the special entries in the keyring username
  keyring_block$username <- fill_special_entry(
    keyring_block$username,
    email_template,
    email_processor
  )

  return(keyring_block)
}

#' Updates entries for every keyring in the yaml list
#'
#' @param yaml_data_as_list yaml document loaded as a nested list
#' @param email_processor function which can resolve email template
#' @param email_template a string specifying the required email address format
#'
#' @return a yaml keyring entry with resolved usernames
#' @export
#'
#' @examples
#' simple_data_update <- function(dataset_block, dataset_defaults) {
#'   dataset_block$default <- "test_value"
#'   return(dataset_block)
#' }
#' test_yaml <- list(
#'   "default_script_path" = ".",
#'   "datasets_to_load" = list(
#'     list("name" = "dataset_one"),
#'     list("name" = "dataset_two")
#'   )
#' )
#' update_list_of_keyrings(test_yaml, simple_data_update)
update_list_of_keyrings <- function(yaml_data_as_list,
                                    email_template,
                                    email_processor) {
  # unpack a few components
  keyring_names <- get_entry_names_from_field(
    yaml_data_as_list$keyring_entries
  )

  yaml_keyring_entries <- yaml_data_as_list$keyring_entries

  # Process each sub-datasets
  for (keyring_name in keyring_names) {
    yaml_keyring_entries[[keyring_name]] <-
      update_keyring_block(
        yaml_keyring_entries[[keyring_name]],
        email_template,
        email_processor
      )
  }

  # Add the newly updated entries back to the overall yaml
  yaml_data_as_list$keyring_entries <- yaml_keyring_entries

  # Return the overral thing
  return(yaml_data_as_list)
}


#' Loads in keyring yaml and try to process special entries
#'
#' @param path_to_yaml_file string with path to yaml file
#' @param custom_email_processor function which can resolve email template
#'
#' @return a prepared yaml dataset for checking keyring entries
#' @export
#'
#' @examples
#' local_folder_yaml <- system.file("extdata", "example_keyring_entries.yaml",
#'   package = "abKeyringR"
#' )
#' default_email_parser <- function(x) {
#'   "hello"
#' }
#' load_keyring_yaml(local_folder_yaml, default_email_parser)
load_keyring_yaml <- function(path_to_yaml_file,
                              custom_email_processor) {
  # Get the yaml data from the file itself.
  # Note, the R yaml package only download first yaml block
  yaml_data_as_list <- yaml::read_yaml(path_to_yaml_file)

  # process the email format entry
  email_format_entry <- yaml_data_as_list$email_format

  # Promote entry names
  yaml_data_as_list <- promote_keyring_entry_names(yaml_data_as_list)

  # Parse the special entries
  yaml_data_as_list <- update_list_of_keyrings(
    yaml_data_as_list,
    email_format_entry,
    custom_email_processor
  )

  return(yaml_data_as_list)
}
