### Ingo Rohlfing: Filtering contradictory cases in truth tables from underyling dataframe

**Motivation behind the function**

When I worked with empirical data in QCA, I found it a little bit tedious to 
identify contradictory cases in a truth table and then look up their
set-membership values in the underlying dataset. The `dcc` option 
that Adrian Dusa built into the `QCA` package at some point simplifies
this process (see below) and is highly welcomed. Because my function
has been sitting on my hard drive for so long and the `dcc` option does
not exactly do what I want to have, I wrote a function myself and share it here.


```r
library(QCA) # needed for illustrating QCA
library(stringr) # needed for executing the function
```

I aim creating a hypothetical dataset with contradictions in rows
that are coded as consistent for Y and as inconsistent for Y.
I directly generate fuzzy-set membership values to avoid the 
calibration stage. It works in the same way with crisp sets. 


```r
set.seed(19) # reproducibility seed
df <- data.frame(A = runif(n = 20, min = 0, max = 1),
                 B = runif(n = 20, min = 0, max = 1),
                 C = runif(n = 20, min = 0, max = 1),
                 Y = runif(n = 20, min = 0, max = 1))
df <- round(df, digits = 2) # rounding to shorten set-membership values
rownames(df) <- LETTERS[1:nrow(df)] # assigning case IDs
```

I now create the truth table that is in its standard form not informative
about contradictions. The inclusion threshold (`incl.cut`) is chosen to create
contradictions for consistent and inconsistent rows.


```r
dftt <- truthTable(df, 
                   outcome = "Y", 
                   n.cut = 1, incl.cut = 0.8,
                   show.cases = T)
```

```r
dftt
```

```
## 
##   OUT: output value
##     n: number of cases in configuration
##  incl: sufficiency inclusion score
##   PRI: proportional reduction in inconsistency
## 
##     A  B  C    OUT    n  incl  PRI   cases    
## 2   0  0  1     0     3  0.748 0.442 G,K,S    
## 3   0  1  0     1     2  0.869 0.676 B,Q      
## 4   0  1  1     0     4  0.766 0.568 A,F,L,O  
## 5   1  0  0     1     3  0.867 0.704 J,M,T    
## 7   1  1  0     0     5  0.789 0.617 C,H,I,N,P
## 8   1  1  1     0     1  0.760 0.441 R
```

The `QCA` package has a `dcc` option in the `truthTable()` command for some time
now, `dcc` standing for *deviant cases for consistency in kind*. These are members of a 
truth table row that are non-members of the outcome, which is the same as saying
it is a contradiction or contradictory case. It shows you what cases are 
contradictions by only including their case labels in the truth table.


```r
truthTable(df, 
           outcome = "Y", 
           n.cut = 1, incl.cut = 0.8,
           show.cases = T, dcc = T)
```

```
## 
##   OUT: output value
##     n: number of cases in configuration
##  incl: sufficiency inclusion score
##   PRI: proportional reduction in inconsistency
##   DCC: deviant cases consistency
## 
##     A  B  C    OUT    n  incl  PRI   DCC
## 2   0  0  1     0     3  0.748 0.442 K,S
## 3   0  1  0     1     2  0.869 0.676 Q  
## 4   0  1  1     0     4  0.766 0.568 O  
## 5   1  0  0     1     3  0.867 0.704    
## 7   1  1  0     0     5  0.789 0.617 C,P
## 8   1  1  1     0     1  0.760 0.441 R
```

This output is informative, but in the next step you might want to lock up the cases
in the dataset and check what the membership values in the condition set and outcome
set are. One could do this by subsetting the dataset by case ID, but this can be
tedious when many cases are contradictions.

The function `tt_contra()` allows you to filter the dataframe for the 
contradictory cases. I am sure it could be written more efficiently, but it
serves its purpose. The ouput is a dataframe covering the contradictory
cases with their membership in the conditions and the outcome. This 
can be done for members of consistent rows and members of inconsistent rows.
The package requires loading the `stringr` package into the library.


```r
tt_contra <- function(DFname, TTname, rowtype){
  if(rowtype == 1){
    # picks case IDs from selected truth table rows (character string)
    cons_cases <- TTname$tt$cases[which(TTname$tt$OUT == 1)]
    # turns character strings into a list
    cons_cases <- str_split(cons_cases, ",")
    # creates a character vector with one entry per case ID
    cons_cases <- unlist(as.list(cons_cases))
    # filters cases from the dataframe
    dftemp <- DFname[rownames(DFname) %in% cons_cases, ]
    # filters non-members of Y = contradictions by defniition
    dftemp <- dftemp[dftemp$Y < 0.5, ]
    # adds information about the type of row to which cases belong
    dftemp$type <- "consistent row"
    return(dftemp)
  }
  if(rowtype == 0){
    cons_cases <- TTname$tt$cases[which(TTname$tt$OUT == 0)]
    cons_cases <- str_split(cons_cases, ",")
    cons_cases <- unlist(as.list(cons_cases))
    dftemp <- DFname[rownames(DFname) %in% cons_cases, ]
    dftemp <- dftemp[dftemp$Y < 0.5, ]
    dftemp$type <- "inconsistent row"
    return(dftemp)
  }
}
```

The input for the function is 

- the name of the dataset: `DFname`,
- the name of the truth table object: `TTname`,
- the type of row you want: `1` for consistent rows; `0` for inconsistent
rows; `"all"` (or any character string) for both combined.

Here is how it works with the hypothetical data and truth table.  
Contradictory cases in consistent rows.


```r
tt_contra(df, dftt, rowtype = 1)
```

```
##     A    B    C    Y           type
## Q 0.4 0.69 0.07 0.28 consistent row
```

Contradictory cases in inconsistent rows.


```r
tt_contra(df, dftt, rowtype = 0)
```

```
##      A    B    C    Y             type
## C 0.65 0.81 0.15 0.35 inconsistent row
## K 0.41 0.40 0.70 0.03 inconsistent row
## O 0.23 0.92 0.98 0.01 inconsistent row
## P 0.90 0.79 0.13 0.09 inconsistent row
## R 0.57 0.70 0.88 0.09 inconsistent row
## S 0.43 0.24 0.60 0.17 inconsistent row
```

[R script](./QCAcontra.R) for this analysis.

**Parameters of analysis**


```r
devtools::session_info()
```

```
## - Session info -------------------------------------------------------------------------------------------------------------------
##  setting  value                       
##  version  R version 3.5.3 (2019-03-11)
##  os       Windows 10 x64              
##  system   x86_64, mingw32             
##  ui       RStudio                     
##  language (EN)                        
##  collate  German_Germany.1252         
##  ctype    German_Germany.1252         
##  tz       Europe/Berlin               
##  date     2019-12-21                  
## 
## - Packages -----------------------------------------------------------------------------------------------------------------------
##  package     * version date       lib source        
##  admisc      * 0.5     2019-11-03 [1] CRAN (R 3.5.3)
##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.5.3)
##  backports     1.1.5   2019-10-02 [1] CRAN (R 3.5.3)
##  callr         3.4.0   2019-12-09 [1] CRAN (R 3.5.3)
##  cli           2.0.0   2019-12-09 [1] CRAN (R 3.5.3)
##  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.5.1)
##  desc          1.2.0   2018-05-01 [1] CRAN (R 3.5.1)
##  devtools      2.2.1   2019-09-24 [1] CRAN (R 3.5.3)
##  digest        0.6.23  2019-11-23 [1] CRAN (R 3.5.3)
##  ellipsis      0.3.0   2019-09-20 [1] CRAN (R 3.5.3)
##  evaluate      0.14    2019-05-28 [1] CRAN (R 3.5.3)
##  fansi         0.4.0   2018-10-05 [1] CRAN (R 3.5.1)
##  fastmap       1.0.1   2019-10-08 [1] CRAN (R 3.5.3)
##  fs            1.3.1   2019-05-06 [1] CRAN (R 3.5.3)
##  glue          1.3.1   2019-03-12 [1] CRAN (R 3.5.3)
##  highr         0.8     2019-03-20 [1] CRAN (R 3.5.3)
##  htmltools     0.4.0   2019-10-04 [1] CRAN (R 3.5.3)
##  httpuv        1.5.2   2019-09-11 [1] CRAN (R 3.5.3)
##  knitr       * 1.26    2019-11-12 [1] CRAN (R 3.5.3)
##  later         1.0.0   2019-10-04 [1] CRAN (R 3.5.3)
##  magrittr      1.5     2014-11-22 [1] CRAN (R 3.5.1)
##  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.5.1)
##  mime          0.7     2019-06-11 [1] CRAN (R 3.5.3)
##  pkgbuild      1.0.6   2019-10-09 [1] CRAN (R 3.5.3)
##  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.5.1)
##  prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.5.1)
##  processx      3.4.1   2019-07-18 [1] CRAN (R 3.5.3)
##  promises      1.1.0   2019-10-04 [1] CRAN (R 3.5.3)
##  ps            1.3.0   2018-12-21 [1] CRAN (R 3.5.2)
##  QCA         * 3.6     2019-11-17 [1] CRAN (R 3.5.3)
##  R6            2.4.1   2019-11-12 [1] CRAN (R 3.5.3)
##  Rcpp          1.0.2   2019-07-25 [1] CRAN (R 3.5.3)
##  remotes       2.1.0   2019-06-24 [1] CRAN (R 3.5.3)
##  rlang         0.4.1   2019-10-24 [1] CRAN (R 3.5.3)
##  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.5.1)
##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.5.1)
##  shiny         1.4.0   2019-10-10 [1] CRAN (R 3.5.3)
##  stringi       1.4.3   2019-03-12 [1] CRAN (R 3.5.3)
##  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 3.5.2)
##  testthat      2.3.1   2019-12-01 [1] CRAN (R 3.5.3)
##  usethis       1.5.1   2019-07-04 [1] CRAN (R 3.5.3)
##  venn          1.7     2018-07-31 [1] CRAN (R 3.5.1)
##  withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.1)
##  xfun          0.10    2019-10-01 [1] CRAN (R 3.5.3)
##  xtable        1.8-4   2019-04-21 [1] CRAN (R 3.5.3)
## 
## [1] C:/Users/Ingo R/Documents/R/win-library/3.5
## [2] C:/Program Files/R/R-3.5.3/library
```

Packages used for analysis.



-   base (R Core Team 2019)
-   grateful (Rodriguez-Sanchez 2018)
-   stringr (Wickham 2019)
-   QCA (Dusa 2019b)
-   admisc (Dusa 2019a)

References
----------

 Dusa, Adrian. 2019a. *admisc: Adrian Dusa's Miscellaneous*.  
 <https://CRAN.R-project.org/package=admisc>.
 
 ---. 2019b. *QCA with R. A Comprehensive Resource*. Cham, Switzerland:  
 Springer International Publishing.
 
 R Core Team. 2019. *R: A Language and Environment for Statistical Computing*.  
 Vienna, Austria: R Foundation for Statistical Computing.  
 <https://www.R-project.org/>.
 
 Rodriguez-Sanchez, Francisco. 2018. *grateful: Facilitate Citation of R Packages*.  
 <https://github.com/Pakillo/grateful>.
 
 Wickham, Hadley. 2019. *stringr: Simple, Consistent Wrappers for Common String Operations*.  
 <https://CRAN.R-project.org/package=stringr>.
