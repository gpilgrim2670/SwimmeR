# SwimmeR
SwimmeR package

SwimmeR is intended to assist those working with times from competative pool swimming races, such as those conducted under the NCAA or FINA.

#Usage
Version 1.0.0 does two things.  First, it converts swimming times (performances) between the computationally useful 
format of seconds, reported to the 100ths place (eg 95.37) and the conventional swimming format (1:35.37).  This is accomlished with `sec_format` and `mmss_format`, which are inverses of one another.

```r
times <- c("1:35.97", "57.34", "16:53.19")
times_sec <- sec_format(times)
times_sec
times_mmss <- mmss_format(times_sec)
times_mmss
all.equal(times, times_mmss)
```

Secondly SwimmeR converts between the various pool sizes used in competative swimming, namely 50m length (LCM), 25m length (SCM) and 25y length (SCY).  This is accomlished with either `convert_courses` or `convert_courses_DF`, both of which have the same basic functionality.  The difference is the `convert_courses_DF` returns a dataframe including the input variables whereas `convet_courses` only returns the converted time(s).  Both functions will take inputs in either seconds or swimming format.

```r
Swim <- tibble(time = c("6:17.53", "59.14", "4:14.32", "16:43.19"), course = c("LCM", "LCM", "SCY", "SCM"), course_to = c("SCY", "SCY", "SCM", "LCM"), event = c("400 Free", "100 Fly", "400 IM", "1650 Free"))

course_convert(time = Swim$time, course = Swim$course, course_to = Swim$course_to, event = Swim$event)

course_convert_DF(time = Swim$time, course = Swim$course, course_to = Swim$course_to, event = Swim$event)
```

#Getting help
If you find bug, please provide a minimal reproducible example at [github][https://github.com/gpilgrim2670/SwimR]. For questions please contact the [creator][gpilgrim2670@gmail.com]
