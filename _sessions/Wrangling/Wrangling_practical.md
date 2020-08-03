Data Wrangling
================
BaselRBootcamp July 2018<br/><a href='https://therbootcamp.github.io'>www.therbootcamp.com</a><br/><a href='https://twitter.com/therbootcamp'>@therbootcamp</a>

<img src="../_image/dplyr_functions.png" width="70%" style="display: block; margin: auto;" />

Overview
--------

In this practical you'll practice "data wrangling" with the `dplyr` and `tidyr` packages (part of the \`tidyverse collection of packages).

By the end of this practical you will know how to:

1.  Change column names, select specific columns
2.  Create new columns based on existing ones
3.  Select specific rows of data based on multiple criteria
4.  Group data and calculate summary statistics
5.  Combine multiple data sets through key columns
6.  Convert data between wide and long formats

Packages
--------

| Package     | Installation                    |
|:------------|:--------------------------------|
| `tidyverse` | `install.packages("tidyverse")` |

Glossary
--------

<table style="width:83%;">
<colgroup>
<col width="6%" />
<col width="11%" />
<col width="65%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Function</th>
<th align="left">Package</th>
<th align="left">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><code>rename()</code></td>
<td align="left"><code>dplyr</code></td>
<td align="left">Rename columns</td>
</tr>
<tr class="even">
<td align="left"><code>select()</code></td>
<td align="left"><code>dplyr</code></td>
<td align="left">Select columns based on name or index</td>
</tr>
<tr class="odd">
<td align="left"><code>filter()</code></td>
<td align="left"><code>dplyr</code></td>
<td align="left">Select rows based on some logical criteria</td>
</tr>
<tr class="even">
<td align="left"><code>arrange()</code></td>
<td align="left"><code>dplyr</code></td>
<td align="left">Sort rows</td>
</tr>
<tr class="odd">
<td align="left"><code>mutate()</code></td>
<td align="left"><code>dplyr</code></td>
<td align="left">Add new columns</td>
</tr>
<tr class="even">
<td align="left"><code>case_when()</code></td>
<td align="left"><code>dplyr</code></td>
<td align="left">Recode values of a column</td>
</tr>
<tr class="odd">
<td align="left"><code>group_by(), summarise()</code></td>
<td align="left"><code>dplyr</code></td>
<td align="left">Group data and then calculate summary statistics</td>
</tr>
<tr class="even">
<td align="left"><code>left_join()</code></td>
<td align="left"><code>dplyr</code></td>
<td align="left">Combine multiple data sets using a key column</td>
</tr>
<tr class="odd">
<td align="left"><code>spread()</code></td>
<td align="left"><code>tidyr</code></td>
<td align="left">Convert long data to wide format - from rows to columns</td>
</tr>
<tr class="even">
<td align="left"><code>gather()</code></td>
<td align="left"><code>tidyr</code></td>
<td align="left">Convert wide data to long format - from columns to rows</td>
</tr>
</tbody>
</table>

### Cheatsheet

<!-- <figure> -->
<!-- <center> -->
<!-- <a href="https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf"> -->
<!--   <img src="https://image.slidesharecdn.com/data-wrangling-cheatsheet-160705210122/95/data-wrangling-with-dplyr-and-tidyr-cheat-sheet-1-638.jpg?cb=1467752577" alt="Trulli" style="width:70%"> -->
<!--   <figcaption>https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf</figcaption></a> -->
<!-- </figure> -->
Examples
--------

``` r
# -----------------------------------------------
# Wrangling with dplyr and tidyr
# ------------------------------------------------

library(tidyverse)    # Load tidyverse for dplyr and tidyr
library(skimr)        # For skim()

# Load baselers data
baselers <- read_csv("https://raw.githubusercontent.com/therbootcamp/baselers/master/inst/extdata/baselers.txt")

# Skim the data
skim(baselers)

# Perform many dplyr operations in a row

baselers %>%
  
  # Change some names
  rename(age_y = age,
         swimming = rhine) %>%
  
  # Only include people over 30
  filter(age_y > 30) %>%
  
  # Calculate some new columns
  mutate(weight_lbs = weight * 2.22,
         height_m = height / 100,
         BMI = weight / height_m ^ 2,
         
         # Make binary version of sex
         sex_bin = case_when(
                      sex == "male" ~ 0,
                      sex == "female" ~ 1),

        # Show when height is greater than 150
        height_lt_150 = case_when(
                                height < 150 ~ 1,
                                height >= 150 ~ 0)) %>%
  
  # Sort in ascending order of sex, then
  #  descending order of age
  arrange(sex, desc(age_y)))


# Calculate grouped summary statistics

baselers_agg <- baselers %>%
  group_by(sex, education) %>%
  summarise(
    age_mean = mean(age_y, na.rm = TRUE),
    income_median = median(income, na.rm = TRUE),
    N = n()
  )
```

Tasks
=====

### Datasets

| File                  | Rows | Columns |
|:----------------------|:-----|:--------|
| `trial_act.csv`       | 2139 | 27      |
| `trial_act_demo_fake` | 2139 | 3       |

A - Getting setup
-----------------

A1. Open your R project. It should already have the folders `0_Data` and `1_Code`. Make sure that the data files listed in the `Datasets` section above are in your `1_Data` folder

A2. Open a new R script. At the top of the script, using comments, write your name and the date. Save it as a new file called `wrangling_practical.R` in the `2_Code` folder.

A3. Using `library()` load the set of packages for this practical listed in the packages section above.

``` r
## NAME
## DATE
## Wrangling Practical

library(XX)     
library(XX)
#...
```

A4. For this practical, we'll use the `trial_act` data, this is the result of a randomized clinical trial comparing the effects of different medications on adults infected with the human immunodeficiency virus. Using the following template, load the data into R and store it as a new object called `trial_act`.

``` r
# Load trial_act.csv from the data folder in your working directory

trial_act <- read_csv(file = "XXX/XXX")
```

A5. Using the same code structure, load the `trial_act_demo_fake.csv` data as a new dataframe called `trial_act_demo_fake`

A6. Take a look at the first few rows of the datasets by printing them to the console.

``` r
# Print trial_act object
trial_act
```

A7. Use the `skim()` function (from the `skimr` package) to get more details on the datasets.

### B - Change column names with rename()

B1. Look at the names of the `trial_act` data with `names()`

B2. Using `rename()`, change the column name `wtkg` in the `trial_act` dataframe to `weight_kg` (to specify that weight is in kilograms). Be sure to assign the result back to `trial_act` to change it!

``` r
# Change the name to weight_kg from wtkg

trial_act <- trial_act %>%
  rename(XX = XX)
```

B3. Look at the names of your `trial_act` dataframe again, do you now see the column `weight_kg`?

B4. Change the column name `age` to `age_y` (to specify that age is in years).

### C - Select columns with select()

C1. Using the `select()` function, select only the column `age` and print the result. Do you see only the age column now?

C2. Create a new dataframe called `CD4_wide` that *only* contains the columns `pidnum`, `arms`, `cd40`, `cd420`, and `cd496`. The `cd40`, `cd420`, and `cd496` columns show patient's CD4 T cell counts at baseline, 20 weeks, and 96 weeks. Print the result to make sure it worked!

``` r
XX <- trial_act %>% 
  select(XX, XX, XX, XX, ...)
```

C3. Did you know you can easily select all columns that start with specific characters using `starts_with()`? Try adapting the following code to get the same result you got before.

``` r
CD4_wide <- trial_act %>% 
  select(pidnum, arms, starts_with("XXX"))
```

### D - Add new columns with mutate()

D1. Using the `mutate()` function, add the column `age_m` which shows each patient's age in months instead of years (Hint: Just multiply `age_y` by 12!)

``` r
trial_act <- trial_act %>%
  mutate(XX = XX * 12)
```

D2. Add the following new columns to `trial_act`. Try combining these into *one* call to the mutate() function!

    - `weight_lb`: Weight in lbs instead of kilograms. You can do this by multiplying `weight_kg` by 2.2.
    - `cd_change_20`: Change in CD4 T cell count from baseline to 20 weeks. You can do this by taking `cd420 - cd40`
    - `cd_change_960`: Change in CD4 T cell count from baseline to 96 weeks. You can do this by taking `cd496 - cd40`

``` r
trial_act <- trial_act %>% 
  mutate(weight_lb = XXX,
         cd_change_20 = XXX,
         XX = XX)
```

D3. If you look at the `gender` column, you will see that it is numeric. Change the column so it shows gender as a string, where0 = "female" and 1 = "male". To do this, use a combination of `mutate()` and `case_when`:

``` r
# Create gender_char which shows gender as a stringh
trial_act <- trial_act %>%
  mutate(
  gender_char = case_when(
    gender == XX ~ "XX",
    gender == XX ~ "XX"))
```

D4. The column `arms` is also numeric and not very meaningful. Change `arms` so that it is a character column indicating the actual names of the trial arms. Here is a table of the mapping

| arms | arms\_char                 |
|:-----|:---------------------------|
| 0    | zidovudine                 |
| 1    | zidovudine and didanosine  |
| 2    | zidovudine and zalcitabine |
| 3    | didanosine                 |

D5. If you haven't already, put the code for your previous questions in one call to `mutate()`. That is, in one block of code, create `agem`, `weight_lb`, `cd_change_20`, `cd_change_960`, `gender_char` and `over50` using the `mutate()` function only once. Here's how your code should look:

``` r
trial_act <- trial_act %>%
  mutate(
    agem = XXX,
    weight_lb = XXX,
    cd_change_20 = XXX,
    cd_change_960 = XXX,
    gender_char = case_when(XXX),
    arms_char = case_when(XXX)
  )
```

### E - Arrange rows with arrange()

E1. Using the `arrange()`function, arrange the `trial_act` data in ascending order of `age_y` (from lowest to highest). After you do, look the data to make sure it worked!

E2. Now arrange the data in *descending* order of `age_y` (from highest to lowest). After, look the data to make sure it worked. To arrange data in descending order, just include `desc()` around the variable. E.g.; `data %>% arrrange(desc(height))`

E3. You can sort the rows of dataframes with multiple columns by including many arguments to `arrange()`. Now sort the data by arms (`arms`) and then age (`age_y`).

### F - Filter specific rows with `filter()`

F1. Using the `filter()` function, create a new dataframe called `trial_act_m` that only contains data from males (`gender_char == "male"`). After you finish, print your new dataframe to make sure it looks correct!

F2. A colleague of yours named Tracy wants a datafame only containing data from females over the age of 40. Create this dataframe with `filter()` and call it `trial_act_Tracy`

``` r
trial_act_Tracy <- trial_act %>%
  filter(agey > XX & gender == XX)
```

### G - Combine dataframes with `left_join()`

G1. The `trial_act_demo_fake.csv` file contains additional (fictional) demographic data about the patients, namely the number of days of exercise they get per week, and their highest level of education. Use the `left_join()` function to combine the `trial_act` and `trial_act_demo_fake` datasets, set the `by` argument to the name of the column that is common in both data sets. Assign the result to `trial_act`.

``` r
trial_act <- trial_act %>%
  left_join(XX, by = "XX")
```

G2. Print your new `trial_act` dataframe. Do you now see the demographic data?

### H - Calculate grouped statistics with `group_by()` and `summarise()`

H1. In this code we'll calculate summary statistics for each of the trial arms. Start with the `trial_act` dataframe. Then, group the data by `arms`. Then, for each arm, calculate the mean participant age (in years) as a new column called `age_mean`. Also, using `N = n()`, calculate the number of cases for each group. Assign the result to a new object called `trial_arm`.

``` r
trial_arm <- trial_act %>% 
  group_by(XX) %>%
  summarise(
    N = n(),
    XX = mean(XX)
  )
```

H2. Adjust your previous code to calculate the standard deviation of age *in addition* to the mean.

H3. Adjust your previous code to calculate the median number of days until the first major negative event (`days`) for each arm.

H4. Create a new dataframe called `trial_gender` groups the data based on `gender` and calculates the same summary statistics as you did for `trial_arm`

H5. Create a new dataframe called `trial_arm_gender` that calculates the same summary statistics for all the groups `gender` and `arms`. Hint: Just add a second grouping variable!

### I - Reshaping with gather() and spread()

I1. Remember the `CD4_wide` dataframe you created before? Currently it is in the wide format, where key data (different CD4 T cell counts) are in different columns. Now we will try to convert it to a long format. Our goal is to get the data in the 'long' format. Using the `spread()` function, create a new dataframe called `CD4_long` that shows the data in the 'long' format. To do this, use the following template. Set the grouping column to `time` and the new data column to `value`.

``` r
CD4_long <- CD4_wide %>% 
  gather(XX,  # New grouping column
         XX,  # New data column
         -pidnum, -arms)  # Names of columns to replicate
```

I2. Print your `CD4_long` dataframe and compare it to the original `CD4_wide`

I3. Now that your data are in the wide format, it should be easy to calculate grouped summary statistics! For each time point and trial arm, calculate the mean CD4 T cell count using `group_by()` and `summarise()`.

I4. Now it's time to practice moving data from the long to the wide format. Using the following template, use the `spread()` function to convert `CD4_long` *back the wide format*. Assign the result to a new object called `CD4_wide_2`.

``` r
CD4_wide_2 <- CD4_long %>% 
  spread(XX,   # old group column
         XX)   # old target column
```

I5. Compare `CD4_wide_2` to `CD4_wide` do they look the same?

Advanced
========

### X - Play around with "Scoped" functions

X1. Many common dplyr functions like `mutate()` and `summarise()` have 'scoped' versions with suffixes like `_if` and `_all`. that allow you do some pretty cool stuff easily (look at the help menu with `?scoped` for details). Try running the following chunk with `summarise_if()` and see what happens:

``` r
# See how summerise_if() works!
baselers %>%
  group_by(sex) %>%
  summarise_if(is.numeric, mean)
```

X2. Now, in the `trial_act` dataset, group the data by `arm` and calculate the mean of all numeric columns using `summerise_if()`. Here's another scoped function in action `mutate_if()` in action:

``` r
# use mutate_if() to round all numeric variables to 2 digits
baselers %>%
  mutate_if(is.numeric, round, 2)
```

X3. Using `mutate_if()`, round all of your results from the previous question to 0 decimal places (to the nearest integer)

### Y - Combine many functions

Y1. For each arm, calculate the following:

-   Mean days until a a major negative event (`days`)
-   Mean CD4 T cell count at baseline. (`cd40`)
-   Mean CD4 T cell count at 20 weeks. (`cd420`)
-   Mean CD4 T cell count at 96 weeks. (`cd496`)
-   Mean *change* in CD4 T cell count between baseline and 96 weeks
-   Number of patients in each arm

Y2. Create the following dataframe that shows patient's mean CD8 T cell count (from columns `cd80` and `cd820`), where the data are grouped by time and trial arm. (Hint: use the following functions in order: select(), gather(), mutate(), group\_by(), summarise())

    # A tibble: 8 x 5
    # Groups:   time [?]
      time     arms_char     N cd8_mean cd8_median
      <chr>    <chr>     <int>    <dbl>      <dbl>
    1 baseline D           561     972.       890 
    2 baseline Z           532     987.       881 
    3 baseline ZD          522    1004.       917 
    4 baseline ZZ          524     984.       898.
    5 week 20  D           561     943.       871 
    6 week 20  Z           532     928.       818 
    7 week 20  ZD          522     968.       903 
    8 week 20  ZZ          524     902.       862 

Additional Resources
--------------------

-   See <https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html> for the full dplyr vignette with lots of wrangling tips and tricks.
