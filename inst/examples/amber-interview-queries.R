library(amberr)

# Start Amber session
a <- amber.login(username = "admin@obiba.org", password = "password", url = "http://localhost:3030")

#
# Interview designs
#
# Find all interview designs
amber.interview_designs(a)
# Find interview designs in a study, by the study name
amber.interview_designs(a, study = "liftup")
# Find a interview design by name
amber.interview_design(a, id = "treocapa_lt")

amber.campaigns(a)
amber.campaigns(a, study = "liftup")
amber.campaign(a, id = "base")

amber.participants(a)
amber.participants(a, study = "liftup")
amber.participants(a, campaign = "base")
amber.participant(a, "9WVV87")

#
# Interviews (raw)
# are collected in the context of one or more study form
# and contain some technical information (state, audit of actions)
# along with the raw data
#
# Find all interviews
amber.interviews(a)
# Find all completed interviews
amber.interviews(a, query = list(state = "completed"))
# Find all interviews of an interview designs
amber.interviews(a, interviewDesign = "treocapa_lt")

#
# Interview records (export)
# are interview exports: data in tabular format along with the data dictionary
# (based on the form's metadata)
#
# Export data from all interviews
amber.interview_export(a)
amber.interview_export(a, query = list(state = "completed"))
# Export data from all interviews in a range of time
amber.interview_export(a, from = "2022-01-12 00:00", to = "2023-12-31")
# Export data from all interviews for a specific participant/patient study identifier
amber.interview_export(a, identifier = "67567567")
# Export data from all interviews for a specific participant code
amber.interview_export(a, code = "OL41ET")
# Export data from all interviews having their identifier matching a regular expression
amber.interview_export(a, query = list(`identifier[$search]` = "^67"))
# Export data from records collected with a study's interview design and campaign
tables <- amber.interview_export(a, study = "liftup", interviewDesign = "treocapa_lt", campaign = "base")
# Result contains both data and dictionary
tables
# Tables are named with the <interview design name>-<form name>-<revision> pattern
names(tables)
# Merge datasets of a study interview step form in different versions if relevant
dplyr::bind_rows(lapply(tables[startsWith(names(tables), "treocapa_lt-treocapa")], function (t) {
  t$data
}))

# End Amber session
amber.logout(a)
