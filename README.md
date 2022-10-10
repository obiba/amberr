# Amber R

[![Build Status](https://travis-ci.com/obiba/amberr.svg?branch=master)](https://travis-ci.com/obiba/amberr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/amberr)](https://cran.r-project.org/package=amberr)

R package for accessing Amber web services:
* extract case reports

## Installation

Requires R 3.x.

```
# Install dependencies
if (!require("httr")) {
  install.package(c("httr"), dependencies=TRUE)
}
# Install from source code repository
devtools::install_github("obiba/amberr")
```

## Usage

Steps:

* open connection to Amber server
* extract and process documents
* close connection with Amber server

Examples: 

* [export case reports](https://github.com/obiba/amberr/blob/master/inst/examples/amber-export.R)

