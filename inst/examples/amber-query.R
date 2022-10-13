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
amber.forms(a, study = "Trauma Registry")
# Find first form with given name
amber.form(a, id = "Adult trauma")
# Find form with specified ID
amber.form(a, id = "61e69a22fea2df2f3108b508")

#
# Form revisions
# are revisions of a study's form
#
# Find all form revisions
amber.form_revisions(a)
# Find all revisions of a form, by its name
amber.form_revisions(a, form = "Adult trauma")
# Find revisions of each study's forms, by the study ID
amber.form_revisions(a, study = "Trauma Registry", limit = 10)
# Find revisions of each study's forms, by the study name
amber.form_revisions(a, study = "Trauma Registry", limit = 100)
# Find all form revisions with a specific revision number
amber.form_revisions(a, query = list(revision = 1))

#
# Case report forms
# are based on a specific or the last revision of a study's form
#
# Find CRFs based on a form, whatever the revision, by the form name
amber.case_report_forms(a, form = "Adult trauma")
# Find CRFs in a study, by the study name
amber.case_report_forms(a, study = "Trauma Registry")
# Find a CRF with a specific form revision (result can be NULL if there are none)
amber.case_report_form(a, form = "maps", revision = 1)
# Find a CRF based on the latest form revision
amber.case_report_form(a, form = "image", revision = NULL)

# End Amber session
amber.logout(a)
