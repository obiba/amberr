library(amberr)

options(verbose = F)
a <- amber.login(username = "admin@obiba.org", password = "password", url = "http://localhost:3030")
a <- amber.login(username = "jim@muse.com", password = "password123", url = "http://localhost:3030")
a

amber.studies(a)
amber.studies(a, df = FALSE)
amber.studies(a, query = list(name = "Trauma Registry"))
amber.studies(a, query = list(`_id` = "617e50749f542d5771d448ad"))
amber.study(a, id = "Trauma Registry")
amber.study(a, id = "617e50749f542d5771d448ad")
amber.studies(a, query = list(createdBy = amber.user(a, id = "admin@obiba.org")$`_id`))

amber.forms(a)
amber.forms(a, query = list(study = "617e50749f542d5771d448ad"))
amber.form(a, id = "Adult trauma")
amber.form(a, id = "61e69a22fea2df2f3108b508")

amber.form_revisions(a, form = "61e69a22fea2df2f3108b508")
amber.form_revisions(a, form = "Adult trauma")
amber.form_revisions(a, study = "617e50749f542d5771d448ad", limit = 10)
amber.form_revisions(a, study = "Trauma Registry", limit = 100)
amber.form_revisions(a, query = list(revision = 10))

amber.case_report_forms(a, form = "61e69a22fea2df2f3108b508")
amber.case_report_forms(a, form = "Adult trauma")
amber.case_report_forms(a, study = "Trauma Registry")

amber.case_report_forms(a, study = "test")
amber.case_report_form(a, form = "maps", revision = 2)
amber.case_report_form(a, form = "image", revision = NULL)

amber.case_report_export(a, form = "Adult trauma")
amber.case_report_export(a, study = "Trauma Registry")

amber.users(a)
amber.users(a, query = list(email = "jim@muse.com"))
amber.users(a, query = list(role = "guest"))
amber.users(a, query = list(isVerified = "false"))
amber.user(a, id = "jim@muse.com")
amber.user(a, id = "615593fdda14d53617df7cc6")
amber.user(a, id = "jim@x.com")
amber.user(a, id = "1234")

amber.groups(a)
amber.group(a, id = "obiba.org")
amber.group(a, id = "615abb061799dd6a2cdbbb10")


amber.logout(a)
