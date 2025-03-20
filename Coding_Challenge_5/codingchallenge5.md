[Click here to view my GitHub
repository](https://github.com/mzb0226/PLPA_6820/tree/main/Coding_Challenge_5)

## **Loading tidyverse**

``` r
# Load tidyverse for data wrangling and visualization
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## **Loading datasets**

``` r
# Read the datasets using relative paths
diversity <- read_csv("DiversityData.csv", show_col_types = FALSE)
metadata <- read_csv("Metadata.csv", show_col_types = FALSE)
```

## **Joining data using left_join**

``` r
# Join datasets by 'Code'
alpha <- left_join(diversity, metadata, by = "Code")

# Check the first few rows
head(alpha)
```

    ## # A tibble: 6 × 9
    ##   Code   shannon invsimpson simpson richness Crop  Time_Point Replicate
    ##   <chr>    <dbl>      <dbl>   <dbl>    <dbl> <chr>      <dbl>     <dbl>
    ## 1 S01_13    6.62       211.   0.995     3319 Soil           0         1
    ## 2 S02_16    6.61       207.   0.995     3079 Soil           0         2
    ## 3 S03_19    6.66       213.   0.995     3935 Soil           0         3
    ## 4 S04_22    6.66       205.   0.995     3922 Soil           0         4
    ## 5 S05_25    6.61       200.   0.995     3196 Soil           0         5
    ## 6 S06_28    6.65       199.   0.995     3481 Soil           0         6
    ## # ℹ 1 more variable: Water_Imbibed <chr>

``` r
# Check column names first
colnames(alpha)
```

    ## [1] "Code"          "shannon"       "invsimpson"    "simpson"      
    ## [5] "richness"      "Crop"          "Time_Point"    "Replicate"    
    ## [9] "Water_Imbibed"

## **Mutate() function and pipe output of one into input of another (%\>%)**

``` r
# Create a new column for Evenness
alpha_even <- alpha %>%
  mutate(Evenness = shannon / log(richness))
```

## **Summarise data to find sd, mean, standard error**

``` r
# Display first few rows
head(alpha_even)
```

    ## # A tibble: 6 × 10
    ##   Code   shannon invsimpson simpson richness Crop  Time_Point Replicate
    ##   <chr>    <dbl>      <dbl>   <dbl>    <dbl> <chr>      <dbl>     <dbl>
    ## 1 S01_13    6.62       211.   0.995     3319 Soil           0         1
    ## 2 S02_16    6.61       207.   0.995     3079 Soil           0         2
    ## 3 S03_19    6.66       213.   0.995     3935 Soil           0         3
    ## 4 S04_22    6.66       205.   0.995     3922 Soil           0         4
    ## 5 S05_25    6.61       200.   0.995     3196 Soil           0         5
    ## 6 S06_28    6.65       199.   0.995     3481 Soil           0         6
    ## # ℹ 2 more variables: Water_Imbibed <chr>, Evenness <dbl>

``` r
# Summarise and pipe
alpha_average <- alpha_even %>%
  group_by(Crop, Time_Point) %>%
  summarise(
    mean_even = mean(Evenness, na.rm = TRUE),  # Mean Evenness
    count = n(),                               # Sample count
    sd_even = sd(Evenness, na.rm = TRUE),      # Standard deviation
    se_even = sd_even / sqrt(count)            # Standard error
  )
```

    ## `summarise()` has grouped output by 'Crop'. You can override using the
    ## `.groups` argument.

``` r
# Display results
head
```

    ## function (x, ...) 
    ## UseMethod("head")
    ## <bytecode: 0x00000268bf84e2a0>
    ## <environment: namespace:utils>

## **Converting data from long format to wide (Pivot)**

``` r
alpha_average2 <- alpha_average %>%
  select(Time_Point, Crop, mean_even) %>%
  pivot_wider(names_from = Crop, values_from = mean_even) %>%
  mutate(
    diff_cotton_even = Cotton - Soil,
    diff_soybean_even = Soybean - Soil
  )

# Display the transformed data
head(alpha_average2)
```

    ## # A tibble: 4 × 6
    ##   Time_Point Cotton  Soil Soybean diff_cotton_even diff_soybean_even
    ##        <dbl>  <dbl> <dbl>   <dbl>            <dbl>             <dbl>
    ## 1          0  0.820 0.814   0.822          0.00602           0.00740
    ## 2          6  0.805 0.810   0.764         -0.00507          -0.0459 
    ## 3         12  0.767 0.798   0.687         -0.0313           -0.112  
    ## 4         18  0.755 0.800   0.716         -0.0449           -0.0833

## **Piping altogether to create a plot**

``` r
# Reshape data to long format for ggplot
alpha_long <- alpha_average2 %>%
  pivot_longer(c(diff_cotton_even, diff_soybean_even), names_to = "diff")

# Generate the line plot
plot <- ggplot(alpha_long, aes(x = Time_Point, y = value, color = diff)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Time Point", y = "Difference in Evenness", color = "Crop Type")
  print(plot)
```

![](codingchallenge5_files/figure-gfm/plot_data-1.png)<!-- -->
