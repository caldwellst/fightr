
<!-- README.md is generated from README.Rmd. Please edit that file -->

fightr
======

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/caldwellst/fightr.svg?branch=master)](https://travis-ci.com/caldwellst/fightr)
[![R build
status](https://github.com/caldwellst/fightr/workflows/R-CMD-check/badge.svg)](https://github.com/caldwellst/fightr/actions)
<!-- badges: end -->

The goal of fightr is to enable quick and reproducible wrangling and
analysis of data in the Fight for Peace database.

Installation
------------

You can install fightr from [CRAN](https://github.com/caldwellst/fightr)
with:

    # install.packages("remotes")

    remotes::install_github("caldwellst/fightr")

Database parsing
----------------

Currently, you can parse the database Excel output only, but more to
come.

    library(fightr)

    parse_ffp_database("filepath.xlsx")
