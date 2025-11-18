## CRAN submission of PHEindicatormethods package version 2.1.1

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

devtools::check_win_* on the following R versions gave no ERRORS or WARNINGS.
There was 1 NOTE from R oldrelease about a possibly invalid url but this has
been checked and is correct and is still functional.

* release:    R version 4.5.2 (2025-10-31 ucrt)
* oldrelease: R version 4.4.3 (2025-02-28 ucrt)
* devel:      R Under development (unstable) (2025-11-13 r89017 ucrt)  

</br>

GitHub actions all passed running R-CMD-check on the following platforms:

* macOS-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (oldrel-1)
* ubuntu-latest (release)
* windows-latest (release)


## CRAN Checks for existing package release

CRAN checks for version v2.1.0 are all OK


## Downstream dependencies

revdepcheck::revdep_check() shows no downstream dependencies
