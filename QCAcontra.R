#' ### Ingo Rohlfing: Filtering contradictory cases in truth tables from underyling dataframe
#' 
#' **Motivation behind the function**
#' 
#' When I worked with empirical data in QCA, I found it a little bit tedious to 
#' identify contradictory cases in a truth table and then look up their
#' set-membership values in the underlying dataset. The `dcc` option 
#' that Adrian Dusa built into the `QCA` package at some point simplifies
#' this process (see below) and is highly welcomed. Because my function
#' has been sitting on my hard drive for so long and the `dcc` option does
#' not exactly do what I want to have, I wrote a function myself and share it here.
#+ message = F
library(QCA) # needed for illustrating QCA
library(stringr) # needed for executing the function

#' I aim creating a hypothetical dataset with contradictions in rows
#' that are coded as consistent for Y and as inconsistent for Y.
#' I directly generate fuzzy-set membership values to avoid the 
#' calibration stage. It works in the same way with crisp sets. 
#+
set.seed(19) # reproducibility seed
df <- data.frame(A = runif(n = 20, min = 0, max = 1),
                 B = runif(n = 20, min = 0, max = 1),
                 C = runif(n = 20, min = 0, max = 1),
                 Y = runif(n = 20, min = 0, max = 1))
df <- round(df, digits = 2) # rounding to shorten set-membership values
rownames(df) <- LETTERS[1:nrow(df)] # assigning case IDs

#' I now create the truth table that is in its standard form not informative
#' about contradictions. The inclusion threshold (`incl.cut`) is chosen to create
#' contradictions for consistent and inconsistent rows.
#+ warning = F
dftt <- truthTable(df, 
                   outcome = "Y", 
                   n.cut = 1, incl.cut = 0.8,
                   show.cases = T)
dftt

#' The `QCA` package has a `dcc` option in the `truthTable()` command for some time
#' now, `dcc` standing for *deviant cases for consistency in kind*. These are members of a 
#' truth table row that are non-members of the outcome, which is the same as saying
#' it is a contradiction or contradictory case. It shows you what cases are 
#' contradictions by only including their case labels in the truth table.
#+ warning = F
truthTable(df, 
           outcome = "Y", 
           n.cut = 1, incl.cut = 0.8,
           show.cases = T, dcc = T)

#' This output is informative, but in the next step you might want to lock up the cases
#' in the dataset and check what the membership values in the condition set and outcome
#' set are. One could do this by subsetting the dataset by case ID, but this can be
#' tedious when many cases are contradictions.
#'
#' The function `tt_contra()` allows you to filter the dataframe for the 
#' contradictory cases. I am sure it could be written more efficiently, but it
#' serves its purpose. The ouput is a dataframe covering the contradictory
#' cases with their membership in the conditions and the outcome. This 
#' can be done for members of consistent rows and members of inconsistent rows.
#' The package requires loading the `stringr` package into the library.
#+  
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

#' The input for the function is 
#' 
#' - the name of the dataset: `DFname`,
#' - the name of the truth table object: `TTname`,
#' - the type of row you want: `1` for consistent rows; `0` for inconsistent
#' rows; `"all"` (or any character string) for both combined.
#' 
#' Here is how it works with the hypothetical data and truth table.  
#' Contradictory cases in consistent rows.
#+
tt_contra(df, dftt, rowtype = 1)

#' Contradictory cases in inconsistent rows.
#+
tt_contra(df, dftt, rowtype = 0)

#' [R script](./QCAcontra.R) and [Markdown file](./QCAcontra.md) for this analysis.
#'
#' **Parameters of analysis**
#+
devtools::session_info()

#' Packages used for analysis.
#+
#' -   base (R Core Team 2019)
#' -   grateful (Rodriguez-Sanchez 2018)
#' -   stringr (Wickham 2019)
#' -   QCA (Dusa 2019b)
#' -   admisc (Dusa 2019a)
#'
#' References
#' ----------
#'
#'  Dusa, Adrian. 2019a. *admisc: Adrian Dusa's Miscellaneous*.  
#'  <https://CRAN.R-project.org/package=admisc>.
#'  
#'  ---. 2019b. *QCA with R. A Comprehensive Resource*. Cham, Switzerland:  
#'  Springer International Publishing.
#'  
#'  R Core Team. 2019. *R: A Language and Environment for Statistical Computing*.  
#'  Vienna, Austria: R Foundation for Statistical Computing.  
#'  <https://www.R-project.org/>.
#'  
#'  Rodriguez-Sanchez, Francisco. 2018. *grateful: Facilitate Citation of R Packages*.  
#'  <https://github.com/Pakillo/grateful>.
#'  
#'  Wickham, Hadley. 2019. *stringr: Simple, Consistent Wrappers for Common String Operations*.  
#'  <https://CRAN.R-project.org/package=stringr>.