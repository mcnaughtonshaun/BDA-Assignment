---
title: "BDA - Assignment 9"
author: "Anonymous"
output: 
  pdf_document: 
    toc: yes
    toc_depth: 1
---

```{r setup, include=FALSE}
# This chunk just sets echo = TRUE as default (i.e. print all code)
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=TRUE, include = FALSE}
library(aaltobda)
library(rstan)
library(markmyassignment)

exercise_path <- "https://github.com/avehtari/BDA_course_Aalto/blob/master/exercises/tests/ex9.yml"
set_assignment(exercise_path)
data("factory")
```

# Decision analysis for the factory data 

## Part 1

I compute the expected utility of one product of each machine. I will be using the profit to compute the utility. Observe that if a machine is sold 94 euros are gained. Otherwise, 106 euros are lost. I first created the utility function.

```{r}
utility <-function( draws = y_pred){
  cont = 0
  for (y in draws){
    if (y<85){
      cont = cont-106
    }
    else {
      cont = cont + 94
    }
  }
  uti = cont / length(draws)
  return(uti)
}
```

I will be ussing the hierarchical model to sample quality measurements of each machine. Then, I will use the ultility function to compute each machines utility.
