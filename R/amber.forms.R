#' Get the forms
#'
#' @title Get the forms
#' @family studies functions
#' @param amber A Amber object
#' @param query The search query
#' @param from From item
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.forms(a, query=list(study="617e50749f542d5771d448ad"), from=0, limit=10)
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.forms <- function(amber, query=list(), from=0, limit=100, df = TRUE) {
  query$`$skip` <- from
  query$`$limit` <- limit
  res <- .get(amber, "form", query = query)
  .reportListMetrics(res)

  if (df) {
    vals <- lapply(res$data, function(val) {
      list(
        `_id` = val$`_id`,
        study = val$study,
        name = val$name,
        description = val$description,
        createdBy = val$createdBy,
        createdAt = val$createdAt,
        updatedAt = val$updatedAt
      )
    })
    dplyr::bind_rows(vals)
  } else {
    res
  }
}

#' Get a form by name or identifier.
#'
#' @title Get a form
#' @family studies functions
#' @param amber A Amber object
#' @param id Form's name or identifier
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.form(a, id = "Adult trauma")
#' amber.form(a, id = "61e69a22fea2df2f3108b508")
#' amber.logout(a)
#' }
#' @export
amber.form <- function(amber, id) {
  query <- list()
  if (regexpr("^[a-zA-Z]+", id) == 1) {
    query$name <- id
  } else {
    query$`_id` <- id
  }
  res <- .get(amber, "form", query = query)
  if (length(res$data) > 0) {
    res$data[[1]]
  } else {
    NULL
  }
}

#' Get the revisions of one or several form(s).
#'
#' @title Get the form revisions
#' @family studies functions
#' @param amber A Amber object
#' @param study Study identifier (name or id), optional.
#' @param form Form identifier (name or id), optional.
#' @param query The search query
#' @param from From item
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.form_revisions(a, form="61e69a22fea2df2f3108b508", from=0, limit=10)
#' amber.form_revisions(a, form="Adult trauma")
#' amber.form_revisions(a, study="Trauma Registry", query = list(revision = 1))
#' amber.form_revisions(a, query = list(revision = 1))
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.form_revisions <- function(amber, study = NULL, form = NULL, query=list(), from=0, limit=100, df = TRUE) {
  studyObj <- NULL
  if (!is.null(study)) {
    studyObj <- amber.study(amber, study)
  }
  formObj <- NULL
  if (!is.null(form)) {
    formObj <- amber.form(amber, form)
  }
  query$`$skip` <- from
  query$`$limit` <- limit
  if (!is.null(studyObj)) {
    query$study <- studyObj$`_id`
  }
  if (!is.null(formObj)) {
    query$form <- formObj$`_id`
  }
  res <- .get(amber, "form-revision", query = query)
  .reportListMetrics(res)

  if (df) {
    vals <- lapply(res$data, function(val) {
      list(
        `_id` = val$`_id`,
        study = val$study,
        form = val$form,
        revision = val$revision,
        comment = val$comment,
        publishedBy = val$publishedBy,
        createdAt = val$createdAt,
        updatedAt = val$updatedAt
      )
    })
    dplyr::bind_rows(vals)
  } else {
    res
  }
}

#' Get a form revision by its form name or identifier and its revision number.
#'
#' @title Get a form
#' @family studies functions
#' @param amber A Amber object
#' @param form Form's name or identifier
#' @param revision Revision number
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.form_revision(a, form = "Adult trauma", revision = 10)
#' amber.form_revision(a, form = "61e69a22fea2df2f3108b508", revision = 10)
#' amber.logout(a)
#' }
#' @export
amber.form_revision <- function(amber, form, revision) {
  formObj <- amber.form(amber, form)
  if (!is.null(formObj)) {
    query <- list(
      form = formObj$`_id`,
      revision = revision
      )
    res <- .get(amber, "form-revision", query = query)
    if (length(res$data) > 0) {
      res$data[[1]]
    } else {
      NULL
    }
  } else {
    NULL
  }
}

