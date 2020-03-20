## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(SwimmeR)
library(rvest)
library(dplyr)
library(ggplot2)
library(scales)

## ----Read_Results, message = FALSE--------------------------------------------
file_path <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")

file_read <- Read_Results(file = file_path)

## ----Read_Results output, message = FALSE-------------------------------------
file_read[294:303]

## ----Swim_Parse, message = FALSE----------------------------------------------
df <-
  Swim_Parse(
    file = file_read,
    typo = c("\n", "Indiana  University", ", University of"),
    replacement = c("\n", "Indiana University", "")
  )

## ----Swim Parse output, message = FALSE---------------------------------------
df[67:69,]

## ----Read_Results html, message = FALSE---------------------------------------
url <- "http://www.nyhsswim.com/Results/Girls/2003/NYS/Single.htm"
url_read <- Read_Results(file = url, node = "pre")

## ----Read_Results html output, message = FALSE--------------------------------
url_read[587:598]

## ----Swim_Parse html, message = FALSE-----------------------------------------
df_1 <- Swim_Parse(file = url_read, avoid = c("NY State Rcd:"))

## ----Swim_Parse html output, message = FALSE----------------------------------
df_1[313:315,]

## ----formatting times---------------------------------------------------------
data(King200Breast)
King200Breast

## ----formatting times 2-------------------------------------------------------
King200Breast <- King200Breast %>% 
  mutate(Time_sec = sec_format(Time),
         Time_swim_2 = mmss_format(Time_sec))

King200Breast

## ----formatted times plot, fig.height = 5, fig.width = 7----------------------
King200Breast %>% 
  ggplot(aes(x = Date, y = Time_sec)) +
  geom_point() +
  scale_y_continuous(labels = scales::trans_format("identity", mmss_format)) +
  theme_bw() +
  labs(y= "Time",
       title = "Lilly King NCAA 200 Breaststroke")

