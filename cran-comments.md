## CRAN submission of PHEindicatormethods package version 2.0.2

## Updates in this release

* Added functionality to `phe_quantile` and `phe_sii` functions, no breaking changes.

## R CMD check results on local installation

Windows 10 install using R 4.2.1:
0 errors | 0 warnings | 0 notes


## Other Test Environments 

devtools::check_win_* on the following R versions gave no ERRORS OR WARNINGS.  
The oldrelease check produced 1 NOTE about possible invalid DOIs, but these 
have been checked manually and confirmed as correct:  
* oldrelease: R version 4.2.3 (2023-03-15 ucrt)
* release:    R version 4.3.2 (2023-10-31 ucrt) 
* devel:      R Under development (unstable) (2024-01-23 r85822 ucrt)


GitHub actions successfully ran R CMD Check on:  

* Windows-latest (release)
* ubuntu-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (oldrel-1)
* macos-latest (release) 


rhub::check() has been run on the following platforms with status OK.  
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (ubuntu-gcc-release)
* Debian Linux, R-devel, clang, ISO-8859-15 locale (debian-clang-devel) 
* Fedora Linux, R-devel, clang, gfortran (fedora-clang-devel)


## CRAN Checks for existing package release

CRAN checks for version v2.0.1 are all OK


## Downstream dependencies

revdepcheck::revdep_check() shows no downstream dependencies
