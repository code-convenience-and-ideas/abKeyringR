#' @import stringr
NULL

#' Gets system user name from system info
#'
#' @return The current users login name
#' @export
#'
#' @examples
#' load_username()
load_username <- function() {
  current_system_user <- Sys.info()[["user"]]
  return(current_system_user)
}

#' Takes a keyring entry and converts it to upper case
#'
#' @param entry_string a string for a keyring entry
#'
#' @return an upper cases version of the keyring entry
#' @export
#'
#' @examples
#' convert_entry_to_uppercase("lower")
convert_entry_to_uppercase <- function(entry_string) {
  uppercase_entry <- stringr::str_to_upper(entry_string)
  return(uppercase_entry)
}

#' Takes an email format, ignores it and returns user name for start of email
#'
#' @param email_format a string specifying the required email address format
#'
#' @return an inferred email prefix from the supplied format type
#' @export
#'
#' @examples
#' specified_email_format <- "first_name.last_name@my_domain.com"
#' generic_email_inferer(specified_email_format)
generic_email_inferer <- function(email_format) {
  current_user_name <- load_username()
  return(current_user_name)
}

#' Pulls out the domain from an email format string
#'
#' @param email_format a string specifying the required email address format
#'
#' @return the domain from an email format string
#' @export
#'
#' @examples
#' specified_email_format <- "first_name.last_name@my_domain.com"
#' extract_email_domain(specified_email_format)
extract_email_domain <- function(email_format) {
  email_domain_match_regex <- r"{(?<=@)([a-zA-Z0-9-._]+)}"
  extracted_email_domain <- stringr::str_extract(
    email_format,
    email_domain_match_regex
  )
  return(extracted_email_domain)
}

#' Infers the user email via a user supplied method and
#'
#' @param email_format a string specifying the required email address format
#' @param method_to_infer_name a function to infer email address start
#'
#' @return a user-specific email to use in keyring entries
#' @export
#'
#' @examples
#' email_start_inference <- function(x) {
#'   "hello"
#' }
#' specified_email_format <- "first_name.last_name@my_domain.com"
#' load_user_email(specified_email_format, email_start_inference)
load_user_email <- function(email_format, method_to_infer_name) {
  start_of_email <- method_to_infer_name(email_format)
  email_domain <- extract_email_domain(email_format)

  combined_user_email <- paste0(start_of_email, "@", email_domain)

  return(combined_user_email)
}

#' Takes an input entry string and replaces certain special entries
#'
#' @param candidate_string A string with possible special entries to replace
#' @param email_format a string specifying the required email address format
#' @param email_inferer a function to infer email address start
#'
#' @return a parse entry with any special values replaced with fixed string
#' @export
#'
#' @examples
#' fill_special_entry(
#'   "{system_user}CustomEnding",
#'   "first_name.last_name@my_domain.com",
#'   function(x) {
#'     "hello"
#'   }
#' )
fill_special_entry <- function(candidate_string, email_format, email_inferer) {
  username_special_string <- r"(\{system_user\})"
  useremail_special_string <- r"(\{system_email\})"

  candidate_needs_username <- stringr::str_detect(
    candidate_string, username_special_string
  )
  candidate_needs_email <- stringr::str_detect(
    candidate_string, useremail_special_string
  )

  altered_entry_string <- candidate_string
  if (candidate_needs_username) {
    current_username <- load_username()
    altered_entry_string <- stringr::str_replace(
      altered_entry_string, username_special_string,
      current_username
    )
  }

  if (candidate_needs_email) {
    current_useremail <- load_user_email(email_format, email_inferer)
    altered_entry_string <- stringr::str_replace(
      altered_entry_string, useremail_special_string,
      current_useremail
    )
  }

  return(altered_entry_string)
}
