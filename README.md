
# SwimmeR <img src="inst/logos/hex_logo.png" width="131px" height="140px" align="right" style="padding-left:10px;background-color:white;" />

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SwimmeR?color=blue)](https://cran.r-project.org/package=SwimmeR)
[![](http://cranlogs.r-pkg.org/badges/grand-total/SwimmeR?color=blue)](https://cran.r-project.org/package=SwimmeR)
[![](http://cranlogs.r-pkg.org/badges/SwimmeR?color=blue)](https://cran.r-project.org/package=SwimmeR)
[![](http://cranlogs.r-pkg.org/badges/last-week/SwimmeR?color=blue)](https://cran.r-project.org/package=SwimmeR)

`SwimmeR` is intended to assist those working with times from competitive pool swimming races, such as those conducted under the NHFS, NCAA, or FINA.  For more information please see `vignette("SwimmeR")`.

### Latest Released Version from CRAN
`install.packages("SwimmeR")`

`library(SwimmeR)`

### Latest Development Version from Github

`devtools::install_github("gpilgrim2670/SwimmeR", build_vignettes = TRUE)`

# Usage

Version 0.5.0 of `SwimmeR` has two major uses - importing results and formatting times.  It also has functions for course conversions and drawing brackets.

## Importing Results

`SwimmeR` reads swimming results into R and outputs tidy dataframes of the results.  `SwimmeR` uses `read_results` to read in either a PDF or HTML file (like a url) and the `swim_parse` or `swim_parse_ISL` function to convert the read file to a tidy dataframe.  Reading .hy3 files is also now possible with `swim_parse`, although .hy3 functionality is still under development and quite buggy.

`read_results` has two arguments, `file`, which is the file path to read in, and `node`, required only for HTML files, this is a CSS node where the results reside.  `node` defaults to `"pre"`, which has been correct in every instance tested thus far.

`swim_parse` has four arguments. `file` is the output of `read_results` and is required.  `avoid` is a list of strings.  Rows of the read in file containing any of those strings will not be included.  `avoid` is optional.  Incorrectly specifying it may lead to nonsense rows in the final dataframe, but will not cause an error.  `typo` and `replacement` work together to fix typos, by replacing them with replacements.  Strings in `typo` will be replaced by strings in `replacement` in element index order - that is the first element of `typo` will be replaced everywhere it appears by the first element of `replacement`.  Typos can cause lost data and nonsense rows.  See `?swim_parse` or the package vignette for more information.

```r
swim_parse(
    read_results(
      "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm",
      node = "pre"
    ),
    typo = c("-1NORTH ROCKL"),
    replacement = c("1-NORTH ROCKL")
  )
```

`swim_parse_ISL` only takes one argument, `file`, the output of `read_results`.

```r
swim_parse_ISL(
    read_results(
      "https://isl.global/wp-content/uploads/2019/10/isl-indianapols-results-day-2-2.pdf")
  )
```

`SwimmeR` can only read files in single column format, not double.  `SwimmeR` also does not capture split times.

### Will work - results in single column
![Will work](inst/extdata/HSEmpireMeet.png)

### Will also work - results in single column
![Will also work](inst/extdata/Texas-Florida-Indiana_image.png)

### Will not work - results in multiple columns
![Will not work](inst/extdata/DoubleColumnPDF.PNG)

## Formatting Times

`SwimmeR` also converts times between the conventional swimming format of minutes:seconds.hundredths (1:35.37) and the computationally useful format of seconds, reported to the 100ths place (e.g. 95.37).  This is accomplished with `sec_format` and `mmss_format`, which are inverses of one another.  Both `sec_format` and `mmss_format` work well with `tidyverse` functions.

```r
times <- c("1:35.97", "57.34", "16:53.19", NA)
times_sec <- sec_format(times)
times_sec
times_mmss <- mmss_format(times_sec)
times_mmss
all.equal(times, times_mmss)
```

## Regularizing team names

Team names are often abbreviated.  Rather than specifying every abbreviation `SwimmeR` provides `get_mode` to make the task simpler.

```
name <- c(rep("Lilly King", 5), rep("James Sullivan", 3))
team <- c(rep("IU", 2), "Indiana", "IUWSD", "Indiana University", rep("Monsters University", 2), "MU")
df <- data.frame(name, team, stringsAsFactors = FALSE)
df %>% 
  group_by(name) %>% 
  mutate(Team = get_mode(team))
```

### Drawing brackets

Brackets for single elimination tournaments can be produced for any number of teams between 5 and 64.  Byes will automatically be included for higher seeds as required.

```r
teams <- c("red", "orange", "yellow", "green", "blue", "indigo", "violet")
round_two <- c("red", "yellow", "blue", "indigo")
round_three <- c("red", "blue")
champion <- "red"
draw_bracket(teams = teams,
            round_two = round_two,
            round_three = round_three,
            champion = champion)
```

### Course conversions

Additionally 'SwimmeR' also converts between the various pool sizes used in competitive swimming, namely 50m length (LCM), 25m length (SCM) and 25y length (SCY).  This is accomplished with either `convert_courses` or `convert_courses_DF`, both of which have the same basic functionality.  The difference is the `convert_courses_DF` returns a dataframe including the input variables whereas `convet_courses` only returns the converted time(s).  Both functions will take inputs in either seconds or swimming format.

```r
swim <- tibble(time = c("6:17.53", "59.14", "4:14.32", "16:43.19"), course = c("LCM", "LCM", "SCY", "SCM"), course_to = c("SCY", "SCY", "SCM", "LCM"), event = c("400 Free", "100 Fly", "400 IM", "1650 Free"))

course_convert(time = swim$time, course = swim$course, course_to = swim$course_to, event = swim$event)

course_convert_DF(time = swim$time, course = swim$course, course_to = swim$course_to, event = swim$event)
```

## Getting help

I do a lot of demos on how to use `SwimmeR` at my blog [Swimming + Data Science](https://pilgrim.netlify.app/).

`SwimmeR` also has a vignette.  Call `vignette("SwimmeR")`.  If you download from github don't forget to set `build_vignettes = TRUE`.

If you find bug, please provide a minimal reproducible example at [github](https://github.com/gpilgrim2670/SwimmeR).
