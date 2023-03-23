# Version 0.14.1
* Updated link per CRAN communication

## Test environments
* local Windows 10 install, R 4.2.0
* local OS X install, R 4.2.0
* win_devel
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.14.0
* Issues per CRAN 2023-03-01 have been corrected
* bug fixes related to updates in dplyr::na_if
* make_lineup function added

## Test environments
* local Windows 10 install, R 4.2.0
* local OS X install, R 4.2.0
* win_devel
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.13.0
* added functionality to parse Splash results
* vectorized course_convert function
* improvements to parsing of Omega results
* various bug fixes, documentation updates etc.

## Test environments
* local Windows 10 install, R 4.0.5
* local OS X install, R 4.0.2
* win_devel
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.12.0
* much improved functionality for parsing Omega results
* testing built for Tokyo 2020 results
* Now capturing reaction times for individual events in Hytek results
* various bug fixes, documentation updates etc.

## Test environments
* local Windows 10 install, R 4.0.5
* local OS X install, R 4.0.2
* win_devel
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.11.0
* added splits_to_lap and splits_to_cumulative and associated testing
* added functionality for parsing Omega results
* improvements in age handling
* various bug fixes, documentation updates etc.

## Test environments
* local Windows 10 install, R 4.0.5
* local OS X install, R 4.0.2
* win_devel
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.10.0
* depreciated course_convert_df function
* removed tibble dependency
* improvements and bug fixes for Hytek and S.A.M.M.S. results

## Test environments
* local Windows 10 install, R 4.0.5
* local OS X install, R 4.0.2
* win_devel
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.9.0
* name_reorder function now accepts lists and has a verbose option
* added correct_split_distance function renaming split columns in data frames with mixed split lengths
* added discard_errors function for cleaning web scrapping results and for export to JumpeR package
* various bug fixes

## Test environments
* local Windows 10 install, R 4.0.2
* local OS X install, R 4.0.2
* win_devel
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes


# Version 0.8.0
* added name_reorder function
* added handling of paralympic codes to swim_parse
* fixes to better align package tests with CRAN policies
* various bug fixes

## Test environments
* local Windows 10 install, R 4.0.2
* local OS X install, R 4.0.2
* win_devel

## R CMD check results

0 errors | 0 warnings | 0 notes


# Version 0.7.2
* addressed testing issues for Debian builds which I think are related to debian/pdftools/poppler issues that are beyond my control

## Test environments
* local Windows 10 install, R 4.0.2
* local OS X install, R 4.0.2
* win_devel
* rhub debian-clang-devel

# Version 0.7.1
* addressed testing issues for Debian builds

## Test environments
* local Windows 10 install, R 4.0.2
* local OS X install, R 4.0.2
* win_devel

# Version 0.7.0
* added functionality for reading S.A.M.M.S. style results
* improvements and simplifications to use of swim_parse function
* various bug fixes 

## Test environments
* local Windows 10 install, R 4.0.2
* local OS X install, R 4.0.2
* win_devel

# Version 0.6.0
* added functionality for reading splits and relay swimmers to swim_parse and swim_parse_ISL
* added fixes to make calls to web resources fail more gracefully
* updates to vignette and documentation
* various bug fixes
* significant updates to improve testing coverage

## Test environments
* local Windows 10 install, R 4.0.2
* local OS X install, R 4.0.2
* win_devel

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.5.0
* added functions for dealing with International Swimming League (ISL) results
* updates to vignettes and documentation

## Test environments
* local Windows 10 install, R 4.0.2
* local OS X install, R 4.0.2
* win_devel

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.4.2

## Changes
* Testing modified to address fault on Linux Debian systems
* Package dependencies for vignette adjusted to address notes on Linux Debian/Fedora and Solaris

## Test environments
* local Windows 10 install, R 4.0.2
* local OS X install, R 4.0.0
* win_devel
* rhub Debian Linux, R-release, GCC

# Version 0.4.1

## Resubmission
Updated link in man/King200Breast.Rd per instructions

## Test environments
* local Windows 10 install, R 4.0.1
* local OS X install, R 4.0.0
* win_devel

# Version 0.4.0

## Changes
Added functions to score swim meets, and also to read in a new type of data, .hy3 files.  Also a fair number of bug fixes.

## Test environments
* local Windows 10 install, R 4.0.1
* local OS X install, R 4.0.0
* win_devel

# Version 0.3.1

## Changes
Terribly sorry about the short duration between submissions.  This is an update to fix a serious bug.
Fixed bug that broke major function read_results for html files (about half of use cases)

## Test environments
* local Windows 10 install, R 4.0.1
* local OS X install, R 4.0.0
* win_devel

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.3.0

## Changes
Added functions to further aid in cleaning swimming data, plus functions to create tournament brackets.  Also made minor bug fixes.

## Test environments
* local Windows 10 install, R 4.0.1
* local OS X install, R 4.0.0
* win_devel

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.2.0

## Resubmission
Removed email address from readme.md per instructions

## Changes
Added functions to read, clean, and parse swimming data from html and pdf sources.  This is a large increase in the functionality of the package.

## Test environments
* local Windows 10 install, R 3.6.3
* local OS X install, R 3.6.3
* win_devel

## R CMD check results

0 errors | 0 warnings | 0 notes

# Version 0.1.0.0
## Resubmission
As requested the version number was reduced to 0.1.0.0 and a standard FOSS (MIT) license was added.
Also as requested examples were added for functions and the package name was written in single quotes ('SwimmeR') in description fields

## Test environments
* local OS X install, R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.
