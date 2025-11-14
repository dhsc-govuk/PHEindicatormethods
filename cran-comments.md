## CRAN submission of PHEindicatormethods package version 2.1.0

## Updates in this release

* No changes to functionality
* Due to organisation restructure the code has moved to a new GitHub repo and 
therefore the DESCRIPTION file has been updated as follows:  

1 The BugReports section has been updated to signpost a new GitHub Issues URL.  
2 The new organisation has been added as the Copyright holder.  


## R CMD check results on local installation

There were no ERRORs, WARNINGS or NOTES on the following local installation:
* Windows 11 install using R 4.4.1


## Other Test Environments 

devtools::check_win_* on the following R versions gave no ERRORS, WARNINGS or 
NOTES:  

* release:    R version 4.5.2 (2025-10-31 ucrt)
* oldrelease:  
* devel:      R Under development (unstable) (2025-11-13 r89017 ucrt)  

</br>

GitHub actions, as set up by usethis::use_github_action("check-standard")
to run R-CMD-check on the following platforms, are all passing:

* macOS-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (oldrel-1)
* ubuntu-latest (release)
* windows-latest (release)


## CRAN Checks for existing package release

CRAN checks for version v2.1.0 are all OK


## Downstream dependencies

revdepcheck::revdep_check() shows no downstream dependencies
