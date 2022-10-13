library(amberr)

# Start Amber session
a <- amber.login(username = "admin@obiba.org", password = "password", url = "http://localhost:3030")

#
# Case report records
# are collected with a study's form revision and are exported along the data dictionary
#
# Find all case reports
tables <- amber.case_report_export(a)
# Find all case reports in a range of time
tables <- amber.case_report_export(a, from = "2022-01-12 00:00", to = "2022-02-13")
# Find all case reports for a specific participant/patient identifier
amber.case_report_export(a, pId = "1231")
# Find all case reports having their identifier matching a regular expression
amber.case_report_export(a, query = list(`data._id[$search]` = "^12"))
# Export records collected with a study's form in a specific version
amber.case_report_export(a, study = "Trauma Registry", form = "Adult trauma", query = list(revision = 6))
# Export records collected with a study's form in all versions used
tables <- amber.case_report_export(a, study = "Trauma Registry", form = "Adult trauma")
# Result contains both data and dictionary
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

# Start Opal session
o <- opal.login(username = "administrator", password = "password", url = "https://opal-demo.obiba.org")
#o <- opal.login(username = "administrator", password = "password", url = "http://localhost:8080")

# Work on a specific table
table <- tables$`Adult trauma-7`

# Decorate the data with the dictionary
data <- dictionary.apply(table$data, variables = table$dictionary$variables, categories = table$dictionary$categories)
data$MECHANISM.INJURY_CAUSE
attributes(data$MECHANISM.INJURY_CAUSE)

if (!opal.project_exists(o, "amber")) {
  opal.project_create(o, project = "amber", database = TRUE)
}
# Save table in Opal
opal.table_save(o, data, "amber", "Adult trauma-7", id.name = "_id", overwrite = TRUE, force = TRUE)

# [optional] Update dictionary in Opal
opal.table_dictionary_update(o, "amber", "Adult trauma-7", table$dictionary$variables, categories = table$dictionary$categories)

# Get back the table from Opal
opal.table_get(o, project = "amber", "Adult trauma-7")

# End Opal session
opal.logout(o)
