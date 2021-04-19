# SwimmeR 0.9.0 - April 19th 2021
* Bug fix in splits
* `name_reorder` now accepts lists
* Improvements to `verbose = TRUE` setting in `name_reorder`
* `correct_split_distance` function added for renaming split columns in data frames with mixed split lengths
* `discard_errors` function added for cleaning web scrapping results

# SwimmeR 0.8.0 - March 16 2021
* Introduces parsing of Para athlete codes inside `swim_parse`
* Bug fixes in splits
* Improved handling of symbols as record designations (!, $ etc.) inside `swim_parse`
* Added `names_reorder` function for dealing with names in Lastname, Firstname order
* Better aligned testing with CRAN policies
* Various documentation updates

# SwimmeR 0.7.3 - March 1 2021
* Fixes bugs in 25 length splits under 10.00 seconds
* Other bug fixes for record designation

# SwimmeR 0.7.2 - January 13 2020
* More testing fixes for Debian builds

# SwimmeR 0.7.1 - January 12 2020
* Testing fixes for Debian builds

# SwimmeR 0.7.0 - January 12 2020
* added functionality to `swim_parse` to parse S.A.M.M.S. style files
* added functionality to `swim_parse` to prevent function from failing when no event names are detected
* broadened event name definitions
* added functionality to `swim_parse` to allow raw-er results to be displayed, including swimming-specific strings like DQ and SCR
* major overhaul to `swim_parse` to lessen the need for `typo` and `replacement` arguments and to simplify their use

# SwimmeR 0.6.0 - November 19 2020
* added functionality to `swim_parse` to output relay swimmers
* added functionality to `swim_parse_ISL` to output relay swimmers
* added functionality to `swim_parse_ISL` to output splits

# SwimmeR 0.5.4 - November 4 2020
* added functionality to output splits to `swim_parse`, works but more testing needed

# SwimmeR 0.5.3 - October 23 2020
* added helper function list_transform to clean up `swim_parse` and other functions

# SwimmeR 0.5.2 - October 21 2020
* fixed issue regarding athletes with many names, or with punctuated names
* fixed issue regarding the use of "*" in front of an athlete's name as a designator
* updates to testing to reflect above

# SwimmeR 0.5.1 - October 20 2020
* fixed issue caused by a change in ISL results format

# SwimmeR 0.5.0
* added `swim_parse_ISL` function, for reading in results from the International Swimming League
* updated vignette
* various documentation updates

# SwimmeR 0.4.2
* CRAN housekeeping - addressed testing issue on Linux Debian

# SwimmeR 0.4.1
* fixed broken link for CRAN submission
* minor bug fixes

# SwimmeR 0.4.0
* important bug fixes in `swim_parse` regarding importing results that have a "J" next to them (a Hy-Tek artifact)
* `swim_parse` now includes DQ swims, and the output of `swim_parse` has a column called `DQ`.
* `swim_parse` now notes exhibition swims, in the column `Exhbition`
* `read_results` and `swim_parse` can now read .hy3 files.  This feature is not yet stable and will receive future updates
* added the `!%in%` function (not in), which can be useful for cleaning results
* added the `results_score` function, which will score the output of `swim_parse` based on user inputted parameters.
* various bug fixes

# SwimmeR 0.3.1
* fixed issue with xml2 import that broke read_results for .html files

# SwimmeR 0.3.0
* added `get_mode` function that returns the most frequently occurring element(s) of a list
* added `draw_bracket` for creating single elimination brackets e.g. for tournaments and shoot-outs
* added aliases so `swim_parse` and `read_results` now work for `Swim_Parse` and `Read_Results`
* updated vignette


# SwimmeR 0.2.0
* added a `NEWS.md` file to track changes to the package.
* added functions `swim_parse` and `read_results` to allow reading in swimming results from .html and .pdf sources
* `sec_format` now works on lists containing `NA` values
* added a vignette

# SwimmeR 0.0.1.0
* a package is born
