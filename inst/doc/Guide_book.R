## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>",
  cache = FALSE, fig.width=7, fig.height=5
)

options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set()


## ----pressure, echo=FALSE, fig.cap="The flowchart of AutoScore", out.width = '75%', fig.align="center"----
knitr::include_graphics("Figure1.png")

## ----basic, eval=FALSE--------------------------------------------------------
#  # From Github
#  install.packages("devtools")
#  library(devtools)
#  install_github(repo = "nliulab/AutoScore", build_vignettes = TRUE)
#  
#  # From CRAN (recommended)
#  install.packages("AutoScore")

## ----library, results = "hide", warning=FALSE, message=FALSE------------------
library(AutoScore)

## -----------------------------------------------------------------------------
data("sample_data")
head(sample_data)

## -----------------------------------------------------------------------------
names(sample_data)[names(sample_data) == "Mortality_inpatient"] <- "label"

## -----------------------------------------------------------------------------
check_data(sample_data)

## -----------------------------------------------------------------------------
set.seed(4)
out_split <- split_data(data = sample_data, ratio = c(0.7, 0.1, 0.2))
train_set <- out_split$train_set
validation_set <- out_split$validation_set
test_set <- out_split$test_set

## -----------------------------------------------------------------------------
ranking <- AutoScore_rank(train_set, ntree = 100)

## -----------------------------------------------------------------------------
AUC <- AutoScore_parsimony(
    train_set,
    validation_set,
    rank = ranking,
    max_score = 100,
    n_min = 1,
    n_max = 20,
    categorize = "quantile",
    quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1)
  )


## ----csv_generate2, eval=FALSE------------------------------------------------
#  write.csv(data.frame(AUC), file = "D:/AUC.csv")

## -----------------------------------------------------------------------------
# Example 1: Top 6 variables are selected
num_var <- 6
final_variables <- names(ranking[1:num_var])

# Example 2: Top 9 variables are selected
num_var <- 9
final_variables <- names(ranking[1:num_var])

# Example 3: Top 6 variables, the 9th and 10th variable are selected
num_var <- 6
final_variables <- names(ranking[c(1:num_var, 9, 10)])

## ----finalvariab2, results = "hide", warning=TRUE, message=FALSE,eval=TRUE,include=FALSE----
num_var <- 6
final_variables <- names(ranking[1:num_var])

## ----weighting,  warning = FALSE----------------------------------------------
cut_vec <- AutoScore_weighting( 
    train_set,
    validation_set,
    final_variables,
    max_score = 100,
    categorize = "quantile",
    quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1)
  )


## -----------------------------------------------------------------------------
## For example, we have current cutoffs of continuous variable: Age 
## ==============  ===========  =====
## variable        interval     point
## ==============  ===========  =====
## Age             <35            0  
##                 [35,49)        7  
##                 [49,76)       17  
##                 [76,89)       23  
##                 >=89          27  

## -----------------------------------------------------------------------------

# Example 1: rounding up to a nice number
cut_vec$Age <- c(35, 50, 75, 90)

# Example 2: changing cutoffs according to clinical knowledge or preference 
cut_vec$Age <- c(25, 50, 75, 90)

# Example 3: combining categories
cut_vec$Age <- c(50, 75, 90)


## ----scoring, warning = FALSE-------------------------------------------------
cut_vec$lactate_mean <- c(0.2, 1, 3, 4)
cut_vec$bun_mean <- c(10, 40)
cut_vec$aniongap_mean <- c(10, 17)
cut_vec$heartrate_mean<- c(70, 98)
scoring_table <- AutoScore_fine_tuning(train_set,
                        validation_set,
                        final_variables,
                        cut_vec,
                        max_score = 100)


## -----------------------------------------------------------------------------
pred_score <- AutoScore_testing(test_set, final_variables, cut_vec, scoring_table, threshold = "best", with_label = TRUE)
head(pred_score)

## ----csv_generate3, eval=FALSE------------------------------------------------
#  write.csv(pred_score, file = "D:/pred_score.csv")

## -----------------------------------------------------------------------------
print_roc_performance(pred_score$Label, pred_score$pred_score, threshold = 50)

## -----------------------------------------------------------------------------
data("sample_data_small")

## -----------------------------------------------------------------------------
set.seed(4)
out_split <- split_data(data = sample_data_small, ratio = c(0.7, 0, 0.3), cross_validation = TRUE)
train_set <- out_split$train_set
validation_set <- out_split$validation_set
test_set <- out_split$test_set

## -----------------------------------------------------------------------------
ranking <- AutoScore_rank(train_set, ntree = 100)

## ----parsi, warning = FALSE---------------------------------------------------
AUC <- AutoScore_parsimony(
    train_set,
    validation_set,
    rank = ranking,
    max_score = 100,
    n_min = 1,
    n_max = 20,
    cross_validation = TRUE,
    categorize = "quantile",
    fold = 10,
    quantiles = c(0, 0.25, 0.5, 0.75, 1), #c(0, 0.05, 0.2, 0.8, 0.95, 1)
    do_trace = FALSE
  )


## ----csv_generate_auc, eval=FALSE---------------------------------------------
#  write.csv(data.frame(AUC), file = "D:/AUC.csv")

## -----------------------------------------------------------------------------
# Example 1: Top 6 variables are selected
num_var <- 6
final_variables <- names(ranking[1:num_var])

# Example 2: Top 9 variables are selected
num_var <- 9
final_variables <- names(ranking[1:num_var])

# Example 3: Top 6 variables, the 9th and 10th variable are selected
num_var <- 6
final_variables <- names(ranking[c(1:num_var, 9, 10)])

## ----finalvariab, results = "hide", warning=FALSE, message=FALSE,eval=TRUE,include=FALSE----
num_var <- 5
final_variables <- names(ranking[1:num_var])

## ----weighting2,  warning = TRUE----------------------------------------------
cut_vec <- AutoScore_weighting( 
    train_set,
    validation_set,
    final_variables,
    max_score = 100,
    categorize = "quantile",
    quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1)
  )


## -----------------------------------------------------------------------------
## For example, we have current cutoffs of continuous variable: Age 
## ==============  ===========  =====
## variable        interval     point
## ==============  ===========  =====
#> bun_mean       <9             0  
#>                [9,43.2)       1  
#>                [43.2,59)      9  
#>                >=59          13  

## -----------------------------------------------------------------------------

# Example 1: rounding up to a nice number
cut_vec$bun_mean <- c(9, 45, 60)

# Example 2: changing cutoffs according to clinical knowledge or preference 
cut_vec$bun_mean <- c(15, 45, 60)

# Example 3: combining categories
cut_vec$bun_mean <- c(45, 60)


## ---- results = "hide", warning=FALSE, message=FALSE,eval=TRUE,include=FALSE----
cut_vec$bun_mean <- c(45, 60)

## ----scoring2, warning = TRUE-------------------------------------------------
cut_vec$lactate_mean <- c(1, 2, 3)
cut_vec$Age <- c(35, 50, 80)
cut_vec$aniongap_mean <- c(8, 12, 18)
cut_vec$resprate_mean <- c(15, 22)
scoring_table <- AutoScore_fine_tuning(train_set,
                        validation_set,
                        final_variables,
                        cut_vec,
                        max_score = 100)


## -----------------------------------------------------------------------------
pred_score <- AutoScore_testing(test_set, final_variables, cut_vec, scoring_table, threshold = "best", with_label = TRUE)
head(pred_score)

## ----csv_generate, eval=FALSE-------------------------------------------------
#  write.csv(pred_score, file = "D:/pred_score.csv")

## -----------------------------------------------------------------------------
print_roc_performance(pred_score$Label, pred_score$pred_score, threshold = 90)

## ----table one, warning = FALSE-----------------------------------------------
compute_descriptive_table(sample_data)

## -----------------------------------------------------------------------------
uni_table<-compute_uni_variable_table(sample_data)
print(uni_table)

## -----------------------------------------------------------------------------
multi_table<-compute_multi_variable_table(sample_data)
print(multi_table)

