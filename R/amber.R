#' Open a connection with Amber and returns an Amber object.  When the two-factor
#' authentication mechanism is enabled, the user will be prompt for one-time password input.
#'
#' @title Opan a connection with Amber
#'
#' @return An Amber object
#' @param username User name in Amber. Can be provided by "amber.username" option.
#' @param password User password in Amber. Can be provided by "amber.password" option.
#' @param url Amber url. Can be provided by "amber.url" option. Secure http (https) connection is required.
#' @param opts Curl options. Can be provided by "amber.opts" option.
#' @examples
#' \dontrun{
#' # login using credentials from amber.username and amber.password options
#' a <- amber.login("https://amber-demo.obiba.org")
#' # login by providing credentials
#' a <- amber.login("administrator", "password", "https://amber-demo.obiba.org")
#' }
#' @export
#' @import jsonlite
amber.login <- function(username=getOption("amber.username", "anonymous"), password=getOption("amber.password", "password"), url=getOption("amber.url"), opts=getOption("mica.opts", list())) {
  if (is.null(url)) stop("Amber url is required", call.=FALSE)
  amberUrl <- url
  if (startsWith(url, "http://") && !startsWith(url, "http://localhost")) {
    stop("Connecting through secure http is required.")
  }
  urlObj <- httr::parse_url(amberUrl)

  amber <- new.env(parent=globalenv())
  # Username
  amber$username <- username
  # Strip trailing slash
  amber$url <- sub("/$", "", amberUrl)
  # Domain name
  amber$name <- urlObj$hostname
  # Version default value
  amber$version <- NA
  class(amber) <- "amber"

  options <- opts
  if (urlObj$hostname == "localhost" && length(options) == 0) {
    options <- list(ssl_verifyhost=F, ssl_verifypeer=F)
  }

  payload <- list(
    strategy = "local",
    email = username,
    password = password
  )
  amber$httrConfig <- config()
  amber$httrConfig$options <- options

  doAuthenticate <- function() {
    POST(.url(amber, "authentication"), content_type("application/json"), accept("application/json"), config = amber$httrConfig,
         body = jsonlite::toJSON(payload, auto_unbox = TRUE), .verbose())
  }

  r <- doAuthenticate()
  if (httr::status_code(r) == 201) {
    amber$auth <- content(r)
  } else if (httr::status_code(r) == 400) {
    code <- readline(prompt = 'Enter 6-digits code: ')
    payload$token <- code
    r <- doAuthenticate()
    if (httr::status_code(r) == 201) {
      amber$auth <- content(r)
    } else {
      stop("Amber authentication failed.")
    }
  } else {
    stop("Amber authentication failed.")
  }
  amber
}

#' Utility method to build urls. Concatenates all arguments and adds a '/' separator between each element
#' @keywords internal
.url <- function(amber, ...) {
  paste(amber$url, ..., sep="/")
}

#' Verbose flag
#' @import httr
#' @keywords internal
.verbose <- function() {
  verbose <- NULL
  if (getOption("verbose", FALSE)) {
    verbose <- httr::verbose()
  }
  verbose
}