library(amberr)

options(verbose = F)
a <- amber.login(username = "admin@obiba.org", password = "password", url = "http://localhost:3030")

a <- amber.login(username = "jim@muse.com", password = "password123", url = "http://localhost:3030")

a$auth
