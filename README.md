
<!-- README.md is generated from README.Rmd. Please edit that file -->

# abKeyringR

<!-- badges: start -->

[![R-CMD-check](https://github.com/code-convenience-and-ideas/abKeyringR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/code-convenience-and-ideas/abKeyringR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/code-convenience-and-ideas/abKeyringR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/code-convenience-and-ideas/abKeyringR?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of abKeyringR is to create a few simple tools which capture
required keyring entries for a project and give some guidance to
building them.

This should save time and help interoperability across teams for
analysis scripts which depend on “secrets”.

## Installation

You can install the development version of abKeyringR like so:

``` r
# You can install thhis package directly from github
devtools::install_github("https://github.com/code-convenience-and-ideas/abKeyringR.git")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(abKeyringR)
# Load an example yaml kept with the package
local_folder_yaml <- system.file("extdata", "example_keyring_entries.yaml",
  package = "abKeyringR"
)
default_email_parser <- function(x) {
  "hello"
}

loaded_keyring_entries <- abKeyringR::load_keyring_yaml(
  local_folder_yaml, default_email_parser
)

# Pull out the datasets from the yaml as a dataframe
keyring_entry_df <- abKeyringR::keyring_yaml_to_df(loaded_keyring_entries)

# Check whether the entries exist in the system and give a warning if not
resulting_missing_entries <- keyring_entry_df |>
  # Add a column for presence of entries first
  abKeyringR::check_table_entries_available() |>
  # Filter to only missing entry and return + raise warning for missing
  abKeyringR::raise_missing_entry_message()
#> Got an error trying to get keyring entry. Will return FALSE
#> Got an error trying to get keyring entry. Will return FALSE
#> Got an error trying to get keyring entry. Will return FALSE
#> Warning in abKeyringR::raise_missing_entry_message(abKeyringR::check_table_entries_available(keyring_entry_df)): Entry secret_one not found. Please set it up appropriately.
#> Service: test, implied username: AlexCustomEnding.
#> Comment from docs: Note, should match your username and password for the service x.
#> Warning in abKeyringR::raise_missing_entry_message(abKeyringR::check_table_entries_available(keyring_entry_df)): Entry secret_two not found. Please set it up appropriately.
#> Service: test, implied username: hello@company.com.
#> Comment from docs: Note, you should have set up an SSHKEY for this service.
#> Warning in abKeyringR::raise_missing_entry_message(abKeyringR::check_table_entries_available(keyring_entry_df)): Entry secret_three not found. Please set it up appropriately.
#> Service: test, implied username: harcoded_user.
#> Comment from docs: You should go the the service and process an appropriate access token.
```
