# SwimmeR 0.13.3 - November 27th 2021 
* improved error handling for empty results in `swim_parse`

# SwimmeR 0.13.2 - November 17th 2021 
* `results_score` no longer requires an Exhibition column
* `results_score` has more informative error messages
* `swim_parse` output columns `Finals_Time` and `Prelims_Time` have been
renamed `Finals` and `Prelims`.  This is a *major change* and will require 
work flows to be adjusted

# SwimmeR 0.13.1 - November 15th 2021 
* now exporting `swim_place` and `dive_place` 
* `swim_place` and `dive_place` made more flexible

# SwimmeR 0.13.0 - November 5th 2021 
* CRAN release 
* contains all dev version updates since SwimmeR 0.12.0

# SwimmeR 0.12.6 - October 19th 2021 
* Splash results now include relay swimmers
* various speed improvements

# SwimmeR 0.12.5 - August 31st 2021 
* Splash results 
* bug fix for empty files 
* Paralympics 2020 results complete

# SwimmeR 0.12.4 - August 31st 2021
* Started Paralympics 2020 results

# SwimmeR 0.12.3 - August 30th 2021
* Moved `swim_parse_ISL` inside of `swim_parse`

# SwimmeR 0.12.2 - August 30th 2021
* `course_convert` is now vectorized, works well in `dplyr` pipes

# SwimmeR 0.12.1 - August 19th 2021
* Initial work in Splash Meet Manager/Swimming Federation of India results

# SwimmeR 0.12.0 - August 13th 2021 
* CRAN release 
* contains all dev version updates since SwimmeR 0.11.0

# SwimmeR 0.11.7 - August 11th 2021
* Tokyo bug fixes re: Mixed and 4x200 relays

# SwimmeR 0.11.6 - August 11th 2021 * ISL season 2 bug fixes

# SwimmeR 0.11.5 - August 5th 2021 
* Reaction times for Hytek results in individual events

# SwimmeR 0.11.4 - August 3rd 2021 
* Heat handling for Tokyo Olympics phase reports

# SwimmeR 0.11.3 - July 26th 2021
* Updates for Tokyo Olympics

# SwimmeR 0.11.2 - July 20th 2021 
* Bug fixes and testing updates for places as "1)"

# SwimmeR 0.11.1 - July 14th 2021 
* Vignette changes to backdoor in NYS results

# SwimmeR 0.11.0 - July 9th 2021, submitted July 14th 
* CRAN release 
* contains all dev version updates since SwimmeR 0.10.0

# SwimmeR 0.10.6 - July 8th 2021 
* dev release 
* `splits_to_cumulative` function added for converting lap splits to cumulative
format

# SwimmeR 0.10.5 - July 1st 2021 
* dev release 
* `splits_to_lap` function added for converting cumulative splits to lap format

# SwimmeR 0.10.4 - June 30th 2021 
* dev release 
* More informative error message  for running `swim_parse` without first
running `read_results`
* Associated testing for new error message

# SwimmeR 0.10.3 - June 18th 2021 * dev release * Handling of DQs in Omega
results improved

# SwimmeR 0.10.2 - June 15th 2021 
* dev release
* Adding support for Omega files from United States Olympic Trials 
* Age formatting has changed slightly to
accommodate Australian Olympic Trials

# SwimmeR 0.10.0 - June 2nd 2021
* CRAN release
* deprecated `course_convert_df`
in favor of `course_convert(verbose = TRUE)`
* all changes from dev version
SwimmeR 0.9.1 to CRAN * Various documentation and file structure updates

# SwimmeR 0.9.1 - May 12th 2021 
* dev release 
* Bug fixes in S.A.M.M.S results
* Improved testing for `course_convert` family * Removed `tibble` dependency from
`course_convert` family and therefore from `SwimmeR` package
* Added support for British IDs in Hytek files
* Added `correct_split_distance` function to switch between splits by 25
and splits by 50 
* Fixed some bugs in splits 
* Various documentation and file structure updates

# SwimmeR 0.9.0 - April 19th 2021 
* CRAN release 
* Bug fix in splits 
* `name_reorder` now accepts lists 
* Improvements to `verbose = TRUE` setting in `name_reorder` 
* `correct_split_distance` function added for renaming split
columns in data frames with mixed split lengths
* `discard_errors` function added for cleaning web scrapping results

# SwimmeR 0.8.0 - March 16 2021 
* CRAN release 
* Introduces parsing of Para athlete codes inside `swim_parse` 
* Bug fixes in splits 
* Improved handling of symbols as record designations (!, $ etc.) 
inside `swim_parse` 
* Added `names_reorder` function for dealing with names in Lastname, Firstname 
order 
* Better aligned testing with CRAN policies 
* Various documentation updates

# SwimmeR 0.7.3 - March 1 2021 
* dev release 
* Fixes bugs in 25 length splits under 10.00 seconds 
* Other bug fixes for record designation

# SwimmeR 0.7.2 - January 13 2020 
* More testing fixes for Debian builds

# SwimmeR 0.7.1 - January 12 2020 
* Testing fixes for Debian builds

# SwimmeR 0.7.0 - January 12 2020 
* CRAN release 
* added functionality to `swim_parse` to parse S.A.M.M.S. style files 
* added functionality to `swim_parse` to prevent function from failing when no
event names are detected
* broadened event name definitions 
* added functionality to `swim_parse` to allow raw-er results to be displayed, 
including swimming-specific strings like DQ and
SCR 
* major overhaul to `swim_parse` to lessen the need for `typo` and
`replacement` arguments and to simplify their use

# SwimmeR 0.6.0 - November 19 2020 
* CRAN release * added functionality to `swim_parse` to output relay swimmers 
* added functionality to `swim_parse_ISL` to output relay swimmers 
* added functionality to `swim_parse_ISL` to output
splits

# SwimmeR 0.5.4 - November 4 2020 
* dev release 
* added functionality to output splits to `swim_parse`, works but more
testing needed

# SwimmeR 0.5.3 - October 23 2020 
* dev release 
* added helper function list_transform to clean up `swim_parse` and
other functions

# SwimmeR 0.5.2 - October 21 2020 
* dev release 
* fixed issue regarding athletes with many names, or with punctuated names 
* fixed issue regarding the use of "*" in front of an athlete's name as a
designator 
* updates to testing to reflect above

# SwimmeR 0.5.1 - October 20 2020 
* CRAN release 
* fixed issue caused by a change in ISL results format

# SwimmeR 0.5.0 # CRAN release 
* added `swim_parse_ISL` function, for reading in
results from the International Swimming League 
* updated vignette 
* various documentation updates

# SwimmeR 0.4.2 
* CRAN housekeeping - addressed testing issue on Linux Debian

# SwimmeR 0.4.1 
* fixed broken link for CRAN submission 
* minor bug fixes

# SwimmeR 0.4.0 
* important bug fixes in `swim_parse` regarding importing
results that have a "J" next to them (a Hy-Tek artifact) 
* `swim_parse` now includes DQ swims, and the output of `swim_parse` has a 
column called `DQ`. 
* `swim_parse` now notes exhibition swims, in the column `Exhbition` 
* `read_results` and `swim_parse` can now read .hy3 files.  This feature is not
yet stable and will receive future updates 
* added the `!%in%` function (not in), which can be useful for cleaning results
* added the `results_score` function, which will score the output of 
`swim_parse` based on user inputted parameters. 
* various bug fixes

# SwimmeR 0.3.1 
* fixed issue with xml2 import that broke read_results for .html files

# SwimmeR 0.3.0 
* added `get_mode` function that returns the most frequently
occurring element(s) of a list 
* added `draw_bracket` for creating single elimination brackets e.g. for 
tournaments and shoot-outs 
* added aliases so `swim_parse` and `read_results` now work for `Swim_Parse` 
and `Read_Results`
* updated vignette

# SwimmeR 0.2.0
* added a `NEWS.md` file to track changes to the package. 
* added functions `swim_parse` and `read_results` to allow reading in swimming
results from .html and .pdf sources 
* `sec_format` now works on lists containing `NA` values 
* added a vignette

# SwimmeR 0.0.1.0 * a package is born
