# gcite

[![Travis-CI Build Status](https://travis-ci.org/muschellij2/gcite.svg?branch=master)](https://travis-ci.org/muschellij2/gcite)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/gcite?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/gcite)
[![Coverage Status](https://img.shields.io/coveralls/muschellij2/gcite.svg)](https://coveralls.io/r/muschellij2/gcite?branch=master)

The goal of gcite is to scrape Google Citation pages and creates word clouds.

## Installation

You can install `gcite` from github with:


``` r
install.packages("devtools")
devtools::install_github("muschellij2/gcite")
```

## Using

You can run the package as follows:

``` r
library("gcite")
out = gcite(author="Matthew Berryman",user="HpLxAzwAAAAJ")
```
where the user ID string comes from the user=ID part of the Google Scholar URL after navigating to a scholar's profile.
