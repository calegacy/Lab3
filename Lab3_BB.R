---
title: "Lab3"
author: "Ben Buzzee"
date: "February 16, 2017"
output: html_document
---

```{r}

rm(list=ls())

library(tidyverse)
setwd("C:/Users/mmor1_000/Desktop/585/Lab3")

file1 <- list()

for (i in 1:4){
  file1[[i]] <- readxl::read_excel("./Spreadsheets/FileOne.xlsx", sheet = i)
}

for (i in 1:4){
  names(file1[[i]])[1:2] <- c("id", "type")
}

for (i in 1:4){
  file1[[i]] <- file1[[i]] %>% mutate(sem=i)
}

#----- File 2 

file2 <- list()

for (i in 1:4){
  file2[[i]] <- readxl::read_excel("./Spreadsheets/FileTwo.xlsx", sheet = i)
}

for (i in 1:4){
  names(file2[[i]])[1:2] <- c("id", "type")
}

for (i in 1:4){
  file2[[i]] <- file2[[i]] %>% mutate(sem=i)
}


sem1 <- left_join(file1[[1]], file2[[1]], by = "id")

sem1$GENDER.x[65:128] <- sem1$GENDER.x[1:64]




```





```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
