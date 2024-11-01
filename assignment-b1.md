Assignment B-1: Making a function
================

# Introduction

This R Markdown file is covered all exercise (1-4) in stat 545 B
“Assignment B-1: Making a function”. This R Markdown file will be
knitted to the pure Markdown file for much easier reading.

# Setup/Preparation

Install and load the packages which will be used in the following
exercise in Assignment B-1 by R.

``` r
#install.packages("dplyr")
#install.packages("testthat")
#install.packages("palmerpenguins")
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

``` r
library(palmerpenguins)
```

# Exercise 1: Make a Function

Based on the experience of the mini-project in STAT545 A, I find “create
a categorical variable from an existing numerical variable” might be a
good idea to make a function. Let’s create a function called
**categorize_by_quantiles**, which can create a categorical variable
with **2 or more groups** from a numerical variable by using quantiles.
To be specific, the number of categories and the label for each category
could be decided by users themselves.

There is the function code:

``` r
# Define the categorize_by_quantiles function 
categorize_by_quantiles <- function(numerical_data, numerical_var_name, categorical_labels, num_categories) {
    # Ensure numerical_data is a data frame or tibble
    if(!(is.data.frame(numerical_data) || inherits(numerical_data, "tbl_df"))){
      stop("Data must be a data frame or tibble!")
    }
    
    # Check if numerical_var_name exists  
    if (!numerical_var_name %in% names(numerical_data)) {
        stop("The specified column name does not exist in the data!")
    }
    
    # Check that the specified column is numeric
    if (!is.numeric(numerical_data[[numerical_var_name]])) {
        stop("The specified column must be numeric!")
    }
    
    # Validate the number of targeted categories is greater than 1
    # Since the number 0 or zero or negative is making no sense
    if (num_categories <= 1) {
        stop("num_categories must be greater than 1 to create meaningful quantile categories!")
    }
    
    # Ensure the length of labels matches num_categories
    if (length(categorical_labels) != num_categories) {
        stop("The length of labels should match num_categories!")
    }
    
    # Calculate quantile-based breaks
    # Allow missing values 
    quantile_breaks <- quantile(numerical_data[[numerical_var_name]], probs = seq(0, 1, length.out = num_categories + 1), na.rm = TRUE)
    
    # Create the new column name
    categorical_col_name <- paste0(numerical_var_name, "_category")
    
    # Use `cut()` to categorize data based on calculated quantile breaks
    numerical_data[[categorical_col_name]] <- cut(
        numerical_data[[numerical_var_name]], 
        breaks = quantile_breaks, 
        labels = categorical_labels, 
        include.lowest = TRUE
    )
    
    return(numerical_data)
}
```

# Exercise 2: Document your Function

In this part, let’s add title, description, arguments and return value
in the created function. <br> Here is the final version:

``` r
#' Categorize a Numeric Variable into Quantile-Based Categories
#'
#' This function takes a numeric column from a data frame or tibble and divides it into 2 or more of categories 
#' by using a specified number of quantiles.
#' A new column with the suffix "_category" is added with the specified labels.
#' The number of categories and the label for each category are decided by users.  
#'
#' @param numerical_data A data frame or tibble containing the numerical column to categorize.
#' @param numerical_var_name A character string of the numerical column name to categorize.
#' @param categorical_labels A character vector of labels for each category. Note: it should match the number of categories specified.
#' @param num_categories An integer specifying the number of categories to split into. Must be greater than 1.
#' @return A data frame or tibble with an additional categorical column based on quantiles.
#' @examples
#' # Using the penguins dataset to categorize body_mass_g
#' categorize_by_quantiles(penguins, "body_mass_g", c("light", "average", "heavy"), 3)
categorize_by_quantiles <- function(numerical_data, numerical_var_name, categorical_labels, num_categories) {
    # Ensure numerical_data is a data frame or tibble
    if(!(is.data.frame(numerical_data) || inherits(numerical_data, "tbl_df"))){
      stop("Data must be a data frame or tibble!")
    }
    
    # Check if numerical_var_name exists  
    if (!numerical_var_name %in% names(numerical_data)) {
        stop("The specified column name does not exist in the data!")
    }
    
    # Check that the specified column is numeric
    if (!is.numeric(numerical_data[[numerical_var_name]])) {
        stop("The specified column must be numeric!")
    }
    
    # Validate the number of targeted categories is greater than 1
    # Since the number 0 or zero or negative is making no sense
    if (num_categories <= 1) {
        stop("num_categories must be greater than 1 to create meaningful quantile categories!")
    }
    
    # Ensure the length of labels matches num_categories
    if (length(categorical_labels) != num_categories) {
        stop("The length of labels should match num_categories!")
    }
    
    # Calculate quantile-based breaks
    # Allow missing values 
    quantile_breaks <- quantile(numerical_data[[numerical_var_name]], probs = seq(0, 1, length.out = num_categories + 1), na.rm = TRUE)
    
    # Create the new column name
    categorical_col_name <- paste0(numerical_var_name, "_category")
    
    # Use `cut()` to categorize data based on calculated quantile breaks
    numerical_data[[categorical_col_name]] <- cut(
        numerical_data[[numerical_var_name]], 
        breaks = quantile_breaks, 
        labels = categorical_labels, 
        include.lowest = TRUE
    )
    
    return(numerical_data)
}
```

# Exercise 3: Include examples

In this part, let’s use a simple tibble and the *penguins* dataset from
the *palemerpenguins* package to show the correct usage and then some
intentional error cases.

### Simple tibble

Let’s firstly define a simple tibble:

``` r
# Define a simple tibble
simple_data <- tibble(
    id = c("1", "2", "3", "4", "5", "6", "7", "8", "9","10"),
    score = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
)

# Display the tibble
print(simple_data)
```

    ## # A tibble: 10 × 2
    ##    id    score
    ##    <chr> <dbl>
    ##  1 1         0
    ##  2 2        10
    ##  3 3        20
    ##  4 4        30
    ##  5 5        40
    ##  6 6        50
    ##  7 7        60
    ##  8 8        70
    ##  9 9        80
    ## 10 10       90

### Correct Usage Examples

Some correct usage examples will be shown below:

#### 1. Using a Simple Tibble

This example divides the **score** column into two categories: “low”,
“high”.

``` r
# Apply categorize_by_quantiles to categorize "score" into two categories
data_categorized_1 <- categorize_by_quantiles(
    numerical_data = simple_data,
    numerical_var_name = "score",
    categorical_labels = c("low", "high"),
    num_categories = 2
)
# Display the result
print(data_categorized_1)
```

    ## # A tibble: 10 × 3
    ##    id    score score_category
    ##    <chr> <dbl> <fct>         
    ##  1 1         0 low           
    ##  2 2        10 low           
    ##  3 3        20 low           
    ##  4 4        30 low           
    ##  5 5        40 low           
    ##  6 6        50 high          
    ##  7 7        60 high          
    ##  8 8        70 high          
    ##  9 9        80 high          
    ## 10 10       90 high

This example divides the **score** column into four categories: “low”,
“medium”, “high”, “very high”.

``` r
# Apply categorize_by_quantiles to categorize "score" into four categories
data_categorized_2 <- categorize_by_quantiles(
    numerical_data = simple_data,
    numerical_var_name = "score",
    categorical_labels = c("low", "medium", "high", "very high"),
    num_categories = 4
)

# Display the result
print(data_categorized_2)
```

    ## # A tibble: 10 × 3
    ##    id    score score_category
    ##    <chr> <dbl> <fct>         
    ##  1 1         0 low           
    ##  2 2        10 low           
    ##  3 3        20 low           
    ##  4 4        30 medium        
    ##  5 5        40 medium        
    ##  6 6        50 high          
    ##  7 7        60 high          
    ##  8 8        70 very high     
    ##  9 9        80 very high     
    ## 10 10       90 very high

#### 2. Using the penguins Dataset

This example uses **penguins** dataset and categorize **body_mass_g**
into three categories: “light”, “average”, “heavy”.

``` r
# Categorize body_mass_g into three quantile-based categories
penguins_categorized <- categorize_by_quantiles(
    numerical_data = penguins,
    numerical_var_name = "body_mass_g",
    categorical_labels = c("light", "average", "heavy"),
    num_categories = 3
)
# Move the new column body_mass_g_category to the front
penguins_categorized <- penguins_categorized %>%
  select(body_mass_g_category, everything())

# Display the result
head(penguins_categorized)
```

    ## # A tibble: 6 × 9
    ##   body_mass_g_category species island    bill_length_mm bill_depth_mm
    ##   <fct>                <fct>   <fct>              <dbl>         <dbl>
    ## 1 average              Adelie  Torgersen           39.1          18.7
    ## 2 average              Adelie  Torgersen           39.5          17.4
    ## 3 light                Adelie  Torgersen           40.3          18  
    ## 4 <NA>                 Adelie  Torgersen           NA            NA  
    ## 5 light                Adelie  Torgersen           36.7          19.3
    ## 6 light                Adelie  Torgersen           39.3          20.6
    ## # ℹ 4 more variables: flipper_length_mm <int>, body_mass_g <int>, sex <fct>,
    ## #   year <int>

``` r
print(penguins_categorized)
```

    ## # A tibble: 344 × 9
    ##    body_mass_g_category species island    bill_length_mm bill_depth_mm
    ##    <fct>                <fct>   <fct>              <dbl>         <dbl>
    ##  1 average              Adelie  Torgersen           39.1          18.7
    ##  2 average              Adelie  Torgersen           39.5          17.4
    ##  3 light                Adelie  Torgersen           40.3          18  
    ##  4 <NA>                 Adelie  Torgersen           NA            NA  
    ##  5 light                Adelie  Torgersen           36.7          19.3
    ##  6 light                Adelie  Torgersen           39.3          20.6
    ##  7 light                Adelie  Torgersen           38.9          17.8
    ##  8 heavy                Adelie  Torgersen           39.2          19.6
    ##  9 light                Adelie  Torgersen           34.1          18.1
    ## 10 average              Adelie  Torgersen           42            20.2
    ## # ℹ 334 more rows
    ## # ℹ 4 more variables: flipper_length_mm <int>, body_mass_g <int>, sex <fct>,
    ## #   year <int>

### Error Examples

There are showing some error examples with using the simple tibble.

#### Error 1: Non-Existent Column

This example attempt to categorize a column which does not exist in the
data.

``` r
data_categorized_error1 <- categorize_by_quantiles(
    numerical_data = simple_data,
    numerical_var_name = "fake_column",
    categorical_labels = c("low", "medium", "high", "very high"),
    num_categories = 4
)
```

    ## Error in categorize_by_quantiles(numerical_data = simple_data, numerical_var_name = "fake_column", : The specified column name does not exist in the data!

``` r
# Expected Error: The specified column name does not exist in the data!
```

#### Error 2: Non-numeric Column

This example attempt to categorize a non-numeric column which just
contains characters.

``` r
data_categorized_error2 <- categorize_by_quantiles(
    numerical_data = simple_data,
    numerical_var_name = "id",
    categorical_labels = c("low", "medium", "high", "very high"),
    num_categories = 4
)
```

    ## Error in categorize_by_quantiles(numerical_data = simple_data, numerical_var_name = "id", : The specified column must be numeric!

``` r
# Expected Error: The specified column must be numeric!
```

#### Error 3: Mismatched labels and num_categories

This example shows an error occurs when the length of labels does not
match **num_categories**.

``` r
data_categorized_error3 <- categorize_by_quantiles(
    numerical_data = simple_data,
    numerical_var_name = "score",
    categorical_labels = c("low", "medium", "high", "very high"),
    num_categories = 3
)
```

    ## Error in categorize_by_quantiles(numerical_data = simple_data, numerical_var_name = "score", : The length of labels should match num_categories!

``` r
# Expected Error: The length of labels should match num_categories!
```

#### Error 4: Invalid data

This example shows an error occurs when **numerical_data** is invalid .

``` r
data_categorized_error4 <- categorize_by_quantiles(
    numerical_data = "1",
    numerical_var_name = "score",
    categorical_labels = c("low", "high"),
    num_categories = 2
)
```

    ## Error in categorize_by_quantiles(numerical_data = "1", numerical_var_name = "score", : Data must be a data frame or tibble!

``` r
# Expected Error: Data must be a data frame or tibble!
```

##### Error 5: Invalid num_categories

This example shows an error occurs when **num_categories** is invalid
(\<=1).

``` r
data_categorized_error5 <- categorize_by_quantiles(
    numerical_data = simple_data,
    numerical_var_name = "score",
    categorical_labels = c("low"),
    num_categories = 1
)
```

    ## Error in categorize_by_quantiles(numerical_data = simple_data, numerical_var_name = "score", : num_categories must be greater than 1 to create meaningful quantile categories!

``` r
# num_categories must be greater than 1 to create meaningful quantile categories!
```

# Exercise 4: Test the Function

In this part, several test cases with different inputs will be shown
below:

``` r
test_that("Testing categorize_by_quantiles function", {
    
    # Sample tibble for testing
    simple_test_data_1 <- tibble(
        id = c("1", "2", "3", "4", "5", "6"),
        score = c(0, 0, 1, 1, 2, 2)
    )
    
    simple_test_data_2 <- tibble(
        id = c("1", "2", "3", "4", "5", "6", "7", "8"),
        score = c(0, 0, 1, 1, 2, 2, NA, NA)
    )
    
    # Show these two test tibbles
    print(simple_test_data_1)
    print(simple_test_data_2)
    
    # Expected Results for tibbles 
    expected_result_1 <- c("low", "low", "medium", "medium", "high", "high")
    expected_result_2 <- c("low", "low", "low", "low", "high", "high", NA, NA)
    
    # 1. Standard Case - 3 categories
    result_1 <- categorize_by_quantiles(simple_test_data_1, "score", c("low", "medium", "high"), 3)
    print(as.character(result_1$score_category))
    expect_true("score_category" %in% names(result_1))
    expect_equal(as.character(result_1$score_category), expected_result_1)
    
    # 2. Edge Case: With NA in numerical column Case
    result_2 <- categorize_by_quantiles(simple_test_data_2, "score", c("low", "high"), 2)
    print(as.character(result_2$score_category))
    expect_true("score_category" %in% names(result_1))
    expect_equal(as.character(result_2$score_category), expected_result_2)
    
    # 3. Error Handling - Non-Existent Column
    # set a non-existent column to the second input variable - numerical_var_name
    expect_error(
      categorize_by_quantiles(simple_test_data_1, "non-existent column", c("low", "medium", "high"), 3),
      "The specified column name does not exist in the data!"
    )
    
    # 4. Error Handling - Non-numeric Column
    # Set a non-numeric column as the input
    expect_error(
      categorize_by_quantiles(simple_test_data_1, "id", c("low", "medium", "high"), 3),
      "The specified column must be numeric!"
    )
    
    # 5. Error Handling - Mismatched labels and num_categories
    # Set an input with length of categorical_labels as 3 but num_categories as 2
    expect_error(
      categorize_by_quantiles(simple_test_data_1, "score", c("low", "medium", "high"), 2),
      "The length of labels should match num_categories!"      
    )    
 
    # 6. Error Handling - Invalid data
    # Set numerical_data as a single character, which is invalid
    expect_error(
      categorize_by_quantiles("simple_test_data_1", "score", c("low", "medium", "high"), 3),
      "Data must be a data frame or tibble!"       
    )  

    # 7. Error Handling - Invalid 'num_categories' (<= 1)
    # Set num_categories = 1, which is an invalid input
    expect_error(
      categorize_by_quantiles(simple_test_data_1, "score", c("low"), 1),
      "num_categories must be greater than 1 to create meaningful quantile categories!"       
    )

})
```

    ## # A tibble: 6 × 2
    ##   id    score
    ##   <chr> <dbl>
    ## 1 1         0
    ## 2 2         0
    ## 3 3         1
    ## 4 4         1
    ## 5 5         2
    ## 6 6         2
    ## # A tibble: 8 × 2
    ##   id    score
    ##   <chr> <dbl>
    ## 1 1         0
    ## 2 2         0
    ## 3 3         1
    ## 4 4         1
    ## 5 5         2
    ## 6 6         2
    ## 7 7        NA
    ## 8 8        NA
    ## [1] "low"    "low"    "medium" "medium" "high"   "high"  
    ## [1] "low"  "low"  "low"  "low"  "high" "high" NA     NA    
    ## Test passed 🌈
