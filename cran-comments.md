## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

--- 
Some of these tests skip on CRAN as Google Scholar doesn't have an API, so you must scrape the website, which is OK with their TOS.  But this causes some rate limits, and so I need to skip them as the IP address from CRAN servers hitting it too much causes 503 errors.