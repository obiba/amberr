#' Get the case reports of one or several form(s).
#'
#' @title Get the case report forms
#' @family studies functions
#' @param amber A Amber object
#' @param study Study identifier (name or id), optional.
#' @param form Form identifier (name or id), optional.
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.case_report_forms(a, form="61e69a22fea2df2f3108b508", skip=0, limit=10)
#' amber.case_report_forms(a, form="Adult trauma")
#' amber.case_report_forms(a, study="Trauma Registry", query = list(revision = 1))
#' amber.case_report_forms(a, query = list(revision = 1))
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.case_report_forms <- function(amber, study = NULL, form = NULL, query=list(), skip=0, limit=100, df = TRUE) {
  studyObj <- NULL
  if (!is.null(study)) {
    studyObj <- amber.study(amber, study)
  }
  formObj <- NULL
  if (!is.null(form)) {
    formObj <- amber.form(amber, form)
  }
  query$`$skip` <- skip
  query$`$limit` <- limit
  if (!is.null(studyObj)) {
    query$study <- studyObj$`_id`
  }
  if (!is.null(formObj)) {
    query$form <- formObj$`_id`
  }
  res <- .get(amber, "case-report-form", query = query)
  .reportListMetrics(res)

  if (df) {
    vals <- lapply(res$data, function(val) {
      list(
        `_id` = val$`_id`,
        study = val$study,
        form = val$form,
        revision = val$revision,
        state = val$state,
        permissions_users = ifelse(is.null(val$permissions) || is.null(val$permissions$users), NA, paste(val$permissions$users, collapse = "|")),
        permissions_groups = ifelse(is.null(val$permissions) || is.null(val$permissions$groups), NA, paste(val$permissions$groups, collapse = "|")),
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

#' Get a case report form by its form name or identifier and its revision number (if any).
#'
#' @title Get a form
#' @family studies functions
#' @param amber A Amber object
#' @param form Form's name or identifier
#' @param revision Revision number, optional, default is NULL (means that the case report form uses the latest form revision).
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.case_report_form(a, form = "Adult trauma", revision = 10)
#' amber.case_report_form(a, form = "61e69a22fea2df2f3108b508")
#' amber.logout(a)
#' }
#' @export
amber.case_report_form <- function(amber, form, revision = NULL) {
  formObj <- amber.form(amber, form)
  if (!is.null(formObj)) {
    query <- list(
      form = formObj$`_id`,
      revision = revision
    )
    res <- .get(amber, "case-report-form", query = query)
    if (length(res$data) > 0) {
      res$data[[1]]
    } else {
      NULL
    }
  } else {
    NULL
  }
}

#' Get the case reports of one or several form(s).
#'
#' @title Get the case report forms
#' @family studies functions
#' @param amber A Amber object
#' @param study Study identifier (name or id), optional.
#' @param form Form identifier (name or id), optional.
#' @param from From date (included), optional
#' @param to To date (included), optional
#' @param pId Patient/participant identifier
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.case_report_export(a, form="61e69a22fea2df2f3108b508", skip=0, limit=10)
#' amber.case_report_export(a, form="Adult trauma", query = list(revision = 7))
#' amber.case_report_export(a, study="Trauma Registry")
#' amber.case_report_export(a, query = list(revision = 1))
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.case_report_export <- function(amber, study = NULL, form = NULL, from = NULL, to = NULL, pId = NULL, query=list(), skip=0, limit=100, df = TRUE) {
  if (!is.null(study)) {
    studyObj <- amber.study(amber, study)
    if (!is.null(studyObj)) {
      query$study <- studyObj$`_id`
    } else {
      stop("No such study with ID or name: ", study, call. = FALSE)
    }
  }
  if (!is.null(form)) {
    formObj <- amber.form(amber, form, study = study)
    if (!is.null(formObj)) {
      query$form <- formObj$`_id`
    } else {
      stop("No such form with ID or name: ", form, call. = FALSE)
    }
  }
  if (!is.null(from)) {
    query$`updatedAt[$gte]` <- .formatDate(from)
  }
  if (!is.null(to)) {
    query$`updatedAt[$lte]` <- .formatDate(to)
  }
  if (!is.null(pId)) {
    query$`data._id` <- pId
  }
  query$`$skip` <- skip
  query$`$limit` <- limit
  res <- .get(amber, "case-report-export", query = query)
  message("tables: ", length(names(res)), " skip: ", skip, " limit: ", limit)

  if (df) {
    out <- list()
    for (name in names(res)) {
      data <- res[[name]]$data
      variables <- res[[name]]$variables

      # make sure every variable has a column (records do not have a field when there is no value for it)
      tbl <- NULL
      for (values in data) {
        row <- list(`_id` = values$`_id`)
        for (variable in variables) {
          row[[variable$name]] <- .unlist(values[[variable$name]])
        }
        tbl <- dplyr::bind_rows(tbl, row)
      }

      # prepare dictionary in tabular format
      out[[name]] <- list(
        dictionary = .makeDictionary(variables),
        data = tbl
      )
    }
    out
  } else {
    res
  }
}
