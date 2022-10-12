#' Get the users
#'
#' @title Get the users
#' @family users functions
#' @param amber A Amber object
#' @param query The search query
#' @param from From item
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.users(a, from=0, limit=10)
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.users <- function(amber, query=list(), from=0, limit=100, df = TRUE) {
  query$`$skip` <- from
  query$`$limit` <- limit
  res <- .get(amber, "user", query = query)
  .reportListMetrics(res)

  if (df) {
    vals <- lapply(res$data, function(val) {
      list(
        `_id` = val$`_id`,
        email = val$email,
        language = val$language,
        role = val$role,
        firstname = val$firstname,
        lastname = val$lastname,
        city = val$city,
        institution = val$institution,
        phone = val$phone,
        title = val$title,
        isVerified = val$isVerified,
        totp2faRequired = val$totp2faRequired,
        totp2faEnabled = val$totp2faEnabled
      )
    })
    dplyr::bind_rows(vals)
  } else {
    res
  }
}

#' Get a user by email or identifier.
#'
#' @title Get a user
#' @family users functions
#' @param amber A Amber object
#' @param id User's email or identifier
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.user(a, id = "jim@muse.com")
#' amber.user(a, id = "6151b512268f582926d37f90")
#' amber.logout(a)
#' }
#' @export
amber.user <- function(amber, id) {
  query <- list()
  if (grepl("@", id, fixed = TRUE)) {
    query$email <- id
  } else {
    query$`_id` <- id
  }
  res <- .get(amber, "user", query = query)
  if (length(res$data) > 0) {
    res$data[[1]]
  } else {
    NULL
  }
}

#' Get the groups
#'
#' @title Get the groups
#' @family users functions
#' @param amber A Amber object
#' @param query The search query
#' @param from From item
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.groups(a, from=0, limit=10)
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.groups <- function(amber, query=list(), from=0, limit=100, df = TRUE) {
  query$`$skip` <- from
  query$`$limit` <- limit
  res <- .get(amber, "group", query = query)
  .reportListMetrics(res)

  if (df) {
    vals <- lapply(res$data, function(val) {
      list(
        `_id` = val$`_id`,
        name = val$name,
        users = paste(val$users, collapse = "|"),
        description = val$description,
        createdAt = val$createdAt,
        updatedAt = val$updatedAt
      )
    })
    dplyr::bind_rows(vals)
  } else {
    res
  }
}

#' Get a user by name or identifier.
#'
#' @title Get a group
#' @family users functions
#' @param amber A Amber object
#' @param id Group's name or identifier
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.group(a, id = "obiba.org")
#' amber.group(a, id = "615abb061799dd6a2cdbbb10")
#' amber.logout(a)
#' }
#' @export
amber.group <- function(amber, id) {
  query <- list()
  if (regexpr("^[a-zA-Z]+", id) == 1) {
    query$name <- id
  } else {
    query$`_id` <- id
  }
  res <- .get(amber, "group", query = query)
  if (length(res$data) > 0) {
    res$data[[1]]
  } else {
    NULL
  }
}