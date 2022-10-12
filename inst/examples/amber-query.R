library(amberr)

# Start Amber session
a <- amber.login(username = "admin@obiba.org", password = "password", url = "http://localhost:3030")
# 2FA is requested if enabled for the user
# a <- amber.login(username = "jim@muse.com", password = "password123", url = "http://localhost:3030")

# Connection handle contains user ID and access token
a

#
# Users
#
# Find all users
amber.users(a)
# Find users with a specific email (only one expected)
amber.users(a, query = list(email = "jim@muse.com"))
# Find users with a specific role
amber.users(a, query = list(role = "guest"))
# Find users not having verified their email
amber.users(a, query = list(isVerified = "false"))
# Find a specific user by email or ID, get NULL if none are found
amber.user(a, id = "jim@muse.com")
amber.user(a, id = "615593fdda14d53617df7cc6")
amber.user(a, id = "jim@x.com")

#
# Groups
#
# Find all groups
amber.groups(a)
# Find group with a specific name or ID
amber.group(a, id = "obiba.org")
amber.group(a, id = "615abb061799dd6a2cdbbb10")

#
# Studies
#
# Find all studies
amber.studies(a)
# Find studies with a specific name
amber.studies(a, query = list(name = "Trauma Registry"))
# Find studies with a specific ID (obviously there is only one)
amber.studies(a, query = list(`_id` = "617e50749f542d5771d448ad"))
# Find first study with a specific name
amber.study(a, id = "Trauma Registry")
# Find study with a specific ID
amber.study(a, id = "617e50749f542d5771d448ad")
# Find studies created by a user
amber.studies(a, query = list(createdBy = amber.user(a, id = "admin@obiba.org")$`_id`))

#
# Forms
# belong to a study
#
# Find all forms
amber.forms(a)
# Find forms from a study
amber.forms(a, query = list(study = "617e50749f542d5771d448ad"))
# Find first form with given name
amber.form(a, id = "Adult trauma")
# Find form with specified ID
amber.form(a, id = "61e69a22fea2df2f3108b508")

#
# Form revisions
# are revisions of a study's form
#
# Find all revisions of a form, by its ID
amber.form_revisions(a, form = "61e69a22fea2df2f3108b508")
# Find all revisions of a form, by its name
amber.form_revisions(a, form = "Adult trauma")
# Find revisions of each study's forms, by the study ID
amber.form_revisions(a, study = "617e50749f542d5771d448ad", limit = 10)
# Find revisions of each study's forms, by the study name
amber.form_revisions(a, study = "Trauma Registry", limit = 100)
# Find all form revisions with a specific revision number
amber.form_revisions(a, query = list(revision = 1))

#
# Case report forms
# are based on a specific or the last revision of a study's form
#
# Find CRFs based on a form, whatever the revision, by the form ID
amber.case_report_forms(a, form = "61e69a22fea2df2f3108b508")
# Find CRFs based on a form, whatever the revision, by the form name
amber.case_report_forms(a, form = "Adult trauma")
# Find CRFs in a study, by the study name
amber.case_report_forms(a, study = "Trauma Registry")
# Find a CRF with a specific form revision (result can be NULL if there are none)
amber.case_report_form(a, form = "maps", revision = 1)
# Find a CRF based on the latest form revision
amber.case_report_form(a, form = "image", revision = NULL)

#
# Case report records
# are collected with a study's form revision and are exported along the data dictionary
#
# Export records collected with a study's form in a specific version
amber.case_report_export(a, study = "Trauma Registry", form = "Adult trauma", query = list(revision = 6))
# Export records collected with a study's form in all versions used
tables <- amber.case_report_export(a, study = "Trauma Registry", form = "Adult trauma")
tables
# Tables are named with the <form name>-<revision> pattern
names(tables)
# Merge datasets from different versions if relevant
dplyr::bind_rows(lapply(tables, function (t) {
  t$data
}))

# End Amber session
amber.logout(a)

#
# Save case report records in Opal
#
library(opalr)
o <- opal.login(username = "administrator", password = "password", url = "https://opal-demo.obiba.org")
# o <- opal.login(username = "administrator", password = "password", url = "http://localhost:8080")

# Work on a specific table
table <- tables$`Adult trauma-7`

# Decorate the data with the dictionary
data <- dictionary.apply(table$data, variables = table$dictionary$variables, categories = table$dictionary$categories)
data$MECHANISM.INJURY_CAUSE

if (!opal.project_exists(o, "amber")) {
  opal.project_create(o, project = "amber", database = TRUE)
}
# Save table in Opal
opal.table_save(o, data, "amber", "Adult_trauma-7", id.name = "_id", overwrite = TRUE, force = TRUE)

# [optional] Update dictionary in Opal
opal.table_dictionary_update(o, "amber", "Adult_trauma-7", table$dictionary$variables, categories = table$dictionary$categories)

# Get back the table from Opal
opal.table_get(o, project = "amber", "Adult_trauma-7")

# End Opal session
opal.logout(o)
