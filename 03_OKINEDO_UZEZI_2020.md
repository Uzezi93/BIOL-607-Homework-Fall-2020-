Sims and Viz
================
Uzezi Okinedo
9/28/2020

``` r
# Load all required libraries
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
library(purrr)
library(ggplot2)
library(readr)
library(forcats)
library(colorfindr)
library(gganimate)
library(transformr)
library(png)
```

## 1\. Sample Properties

Consider the following vasopressin levels in voles.

``` r
# Assign a variable name to vasopressin levels
vole_vaso <- c(98,96,94,88,86,82,77,74,70,60,
           59,52,50,47,40,35,29,13,6,5)
```

### 1a. Say “Vole vasopressin” 10 times as fast as you can. How many times did you trip up?

I tripped up 3 times.

### 1b. What is the mean, median, sd, and interquartile range of the sample?

``` r
# Create a data frame 
as.data.frame(vole_vaso) %>%
  # Summarize mean, median, sd and interquartile range into a data frame
  summarise(mean_vole_vaso = mean(vole_vaso), med_vole_vaso = median(vole_vaso), sd_vole_vaso = sd(vole_vaso), IQR(vole_vaso))
```

    ##   mean_vole_vaso med_vole_vaso sd_vole_vaso IQR(vole_vaso)
    ## 1          58.05          59.5     29.75244          44.25

The mean, median and IQR of the sample are 58.05, 59.5 and 44.25
respectively.

### 1c. What is the standard error of the mean (do this with a formula\!)?

``` r
# Calculate the standard error of sample 
sd(vole_vaso)/sqrt(length(vole_vaso))
```

    ## [1] 6.652849

The standard error of the sample is 6.652849

### 1d. What does the standard error of the mean tell you about our estimate of the mean values of the population of vole vassopressin?

The calculated standard error of the mean shows that the population mean
values are dispersed and far from the sample mean.

## 2\. Sample Size for upper quartiles.

We can get the upper quartile value of vole vassopressin with

``` r
# Calculate the upper qiartile of sample
quantile(vole_vaso, probs = 0.75)
```

    ## 75% 
    ##  83

### 2a. Use sample() to get just one resample with a sample size of 10. What is its upper quartile?

``` r
# Create one resample using sample()
one_resamp <-  sample(vole_vaso,
                    size = length(1:10),
                    replace = TRUE)

one_resamp
```

    ##  [1]  5 82 40 59 94 77 59 47 29 96

``` r
# Calculate upper_quartile of one resample
quantile(one_resamp, probs = 0.75)
```

    ##   75% 
    ## 80.75

The Upper quartile is 92.5

### 2b. Build an initial data frame for simulations with the sample sizes 5 through 20.

``` r
# Create a data frame for simulating sample sizes 5:20
sim_data <- data.frame(samp_size = 5:20) %>%
  # for each sample size
  rowwise(samp_size) %>%
  #replicate (1000 times)
  summarise(sim_data = sample(vole_vaso,
                   size = samp_size,
                   replace = TRUE))
```

    ## `summarise()` regrouping output by 'samp_size' (override with `.groups` argument)

``` r
sim_data
```

    ## # A tibble: 200 x 2
    ## # Groups:   samp_size [16]
    ##    samp_size sim_data
    ##        <int>    <dbl>
    ##  1         5       52
    ##  2         5       74
    ##  3         5       47
    ##  4         5       13
    ##  5         5       50
    ##  6         6       94
    ##  7         6       47
    ##  8         6        6
    ##  9         6       86
    ## 10         6       50
    ## # ... with 190 more rows

### 2c. Use this data frame to get simulated upper quartiles for each sample size. using 1,000 simulations

``` r
sim_upper_quarts <- sim_data %>% 
  # For each sample size (set of params)...
  rowwise() %>%
  
  # Replicate calculating estimated parameters 
  # from a random draw 
  # some # of times
  summarize(upper_quarts = replicate(1000,
                                  sample(vole_vaso,
                                         size = samp_size,
                                        replace = TRUE) %>% 
                                    quantile(probs = 0.75)))
```

    ## `summarise()` regrouping output by 'samp_size' (override with `.groups` argument)

``` r
sim_upper_quarts
```

    ## # A tibble: 200,000 x 2
    ## # Groups:   samp_size [16]
    ##    samp_size upper_quarts
    ##        <int>        <dbl>
    ##  1         5           94
    ##  2         5           98
    ##  3         5           50
    ##  4         5           96
    ##  5         5           74
    ##  6         5           96
    ##  7         5           50
    ##  8         5           52
    ##  9         5           96
    ## 10         5           82
    ## # ... with 199,990 more rows

### 2d. With a ggplot, make a guesstimate as to the best sample size for estimating the upper quartile of the population. Use whatever geom you feel makes things most easy to see. E.C. Add a red dashed line using geom\_vline() or geom\_hline() to show where that should be, perhaps.

``` r
# Create an object for plotting simulated upper quartiles
plot_sim_upper_quarts <- ggplot(data = sim_upper_quarts,
                        mapping = aes(x = samp_size,
                                      y = upper_quarts))

plot_sim_upper_quarts +
  # This geom made the plot readable and quite understandable
  geom_count(bins = 50) +
  # Plot labels
  labs(x = "Sample size", y = "upper_quarts",
       title = "Guesstimate as to the best sample size for estimating the upper quartile") +
  # Added a verical red line to identify the best guesstimate
  geom_vline(xintercept = 17, linetype="dotted", 
                color = "red", size=1.5) +
  theme_bw()
```

    ## Warning: Ignoring unknown parameters: bins

![](03_OKINEDO_UZEZI_2020_files/figure-gfm/a%20guesstimate%20as%20to%20the%20best%20sample%20size%20for%20estimating%20the%20upper%20quartile%20of%20the%20population-1.png)<!-- -->

### 2e. Plot the SE of the estimate of the upper quantile by sample size. Again, what it the best way to see this? Does it level off? Is there a level you feel acceptable? Justify your answer. Does this match with what you put in 3d?

``` r
upper_quart_SE <- sim_upper_quarts %>% 
  # For each sample size (set of params)...
  rowwise() %>%
  
  # Replicate calculating estimated parameters 
  # from a random draw 
  # some # of times
  summarise(upper_quart_SE = sample(vole_vaso,
                        size = samp_size,
                        replace = TRUE)%>%
                        sd(upper_quarts)/sqrt(length(upper_quarts)))
```

    ## `summarise()` regrouping output by 'samp_size' (override with `.groups` argument)

``` r
upper_quart_SE
```

    ## # A tibble: 200,000 x 2
    ## # Groups:   samp_size [16]
    ##    samp_size upper_quart_SE
    ##        <int>          <dbl>
    ##  1         5           26.8
    ##  2         5           34.0
    ##  3         5           32.5
    ##  4         5           42.6
    ##  5         5           30.2
    ##  6         5           23.5
    ##  7         5           30.6
    ##  8         5           19.4
    ##  9         5           16.7
    ## 10         5           30.0
    ## # ... with 199,990 more rows

``` r
# Create an object for plotting SE of upper quartiles

plot_upper_quart_SE <- ggplot(data = upper_quart_SE,
                        mapping = aes(x = samp_size,
                                      y = upper_quart_SE))

plot_upper_quart_SE +
  geom_count(bins = 50) +
  #plot labels
  labs(x = "Sample size", y = "Count",
       title = "SE of the estimate of the upper quantile by sample size") +
  # Added a verical red line to identify the best guesstimate
  geom_vline(xintercept = 17, linetype="dotted", 
                color = "red", size=1.5) +
  theme_bw()
```

    ## Warning: Ignoring unknown parameters: bins

![](03_OKINEDO_UZEZI_2020_files/figure-gfm/plot%20standard%20error%20of%20upper%20quartiles-1.png)<!-- -->

This plot levels off with the plot in 2d. Sample size = 5 has more
standard errors as indicated by the number of counts in the plot. On the
other hand, Sample size = 17 is a good guesstimate for estimating upper
quartiles and has the lowest count of standard errors.

## 3\. Ggplot

### 3a. Some setup. Run the code below. For extra credit, look up the packages and functions used and explain what is going on here. But, that’s EC.

``` r
# Downloaded data from BIOL 607 GitHub course repository and saved in a file in my project directory
download.file(url = "https://biol607.github.io/homework/data/NH_seaice_extent_monthly_1978_2016.csv",
              destfile = "raw_data/seaice.csv")

theme_set(theme_bw(base_size=12))

ice <- read_csv("http://biol607.github.io/homework/data/NH_seaice_extent_monthly_1978_2016.csv") %>%
  mutate(Month_Name = factor(Month_Name),
         Month_Name = fct_reorder(Month_Name, Month))
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Month = col_double(),
    ##   Day = col_double(),
    ##   Extent = col_double(),
    ##   Missing = col_double(),
    ##   Month_Name = col_character()
    ## )

``` r
ice
```

    ## # A tibble: 458 x 6
    ##     Year Month   Day Extent Missing Month_Name
    ##    <dbl> <dbl> <dbl>  <dbl>   <dbl> <fct>     
    ##  1  1978    11     1  10.7        0 Nov       
    ##  2  1978    12     1  12.7        0 Dec       
    ##  3  1979     2     1  15.9        0 Feb       
    ##  4  1979     3     1  16.6        0 Mar       
    ##  5  1979     6     1  13.1        0 Jun       
    ##  6  1979     7     1  11.6        0 Jul       
    ##  7  1979     9     1   7.23       0 Sep       
    ##  8  1979    10     1   7.40       0 Oct       
    ##  9  1980     1     1  14.2        0 Jan       
    ## 10  1980     3     1  16.2        0 Mar       
    ## # ... with 448 more rows

The Month\_Name in the data set was changed to a factor with the factor
function, while the fct\_reorder function reorders one variable for the
other.

### 3b. Make a boxplot showing the variability in sea ice extent every month.

``` r
# Made an object for plotting variability in sea ice extent
var_ice <- ggplot(data = ice,
                        mapping = aes(x = Month,   # Setting ggplot parameters
                                      y = Extent,
                                      fill = Month_Name))
var_ice +
  geom_boxplot() +
  #Plot labels
labs(x ="Month", y ="Sea ice extent",
     title = "Variability in sea ice extent every month") +
  # Here comes the gganimate code
  transition_states(
    Month_Name,
    transition_length = 2,  # Parameters for gganimate
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')
```

![](03_OKINEDO_UZEZI_2020_files/figure-gfm/boxplot-1.gif)<!-- -->

### 3c. Use dplyr to get the annual minimum sea ice extent. Plot minimum ice by year. What do you observe?

``` r
# Create a data frame for minimum sea ice and summarized into a new data frame
min_ice_extent <- ice %>% 
  group_by(Year) %>%
 summarise(min_sea_ice = min(Extent))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
min_ice_extent
```

    ## # A tibble: 39 x 2
    ##     Year min_sea_ice
    ##    <dbl>       <dbl>
    ##  1  1978       10.7 
    ##  2  1979        7.23
    ##  3  1980        7.54
    ##  4  1981        7.20
    ##  5  1982        7.40
    ##  6  1983        7.32
    ##  7  1984        6.83
    ##  8  1985        6.72
    ##  9  1986        7.23
    ## 10  1987        6.89
    ## # ... with 29 more rows

``` r
# Create a dataframe for plotting the extent of minimum sea ice and set ggplot parameters
plot_min_sea_ice <- ggplot(data = min_ice_extent,
                    mapping = aes(x = Year,
                                  y = min_sea_ice))

plot_min_sea_ice +
  geom_point() +
  geom_smooth() +
labs(x ="Year", y ="Minimum sea ice extent",
     title = "Annual minimum sea ice extent")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](03_OKINEDO_UZEZI_2020_files/figure-gfm/plot%20annual%20minimum%20sea%20ice%20extent-1.png)<!-- -->

Lowest sea ice extent is observed in 2009 and continues through year
2010 and beyond.

### 3d. With the original data, plot sea ice by year, with different lines (oh\! What geom will you need for that?) for different months. Then, use facet\_wrap and cut\_interval(Month, n=4) to split the plot into seasons.

``` r
# Create an object for plotting sea ice by year and set parameters 
plot_ice_by_year <- ggplot(data = ice,
                    mapping = aes(x = Extent,
                                  y = Year,
                                  color = Month_Name))

plot_ice_by_year +
  geom_point() +
  # using facet_wrap and cut_interval(Month, n=4) to split the plot into seasons
  facet_wrap(~cut_interval(1:458, n = 4)) +
  labs(x ="Sea ice extent", y ="Year",
     title = "Sea ice by year")
```

![](03_OKINEDO_UZEZI_2020_files/figure-gfm/plot%20sea%20ice%20by%20year-1.png)<!-- -->

### 3e. Last, make a line plot of sea ice by month with different lines as different years. Gussy it up with colors by year, a different theme, critical values, and whatever other annotations, changes to axes, etc., you think best show the story of this data. For ideas, see the lab, and look at various palettes around. Extra credit for using colorfindr to make a palette.

``` r
# Using colorfindr to create a color palette
palette <- get_colors("raw_data/website-color-palettes-18.jpg") %>% # I referenced a jpg file on my computer.
  make_palette(n = 5) # here we extract 5 colors
```

![](03_OKINEDO_UZEZI_2020_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# Creating an object for plotting sea by month with reference to different years
ice_by_month <- ggplot(data = ice,
                        mapping = aes(x = Month_Name,
                                      y = Extent,
                                      group = Year,  # Setting ggplot parameters
                                      colour = Year))
plot_ice_by_month <- ice_by_month +
  geom_line() +
  scale_fill_manual(values = palette) +
  # Plot labels
labs(x ="Month", y ="Sea ice extent",
     title = "sea ice by month with different lines as different years")

plot_ice_by_month
```

![](03_OKINEDO_UZEZI_2020_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

### 3f. Extra Credit. Make it animated with gganimate. Just like above.

``` r
# Applying gganimate
plot_ice_by_month +
  # Here comes the gganimate specific bits
  labs(title = 'sea ice by month with different lines as different years', x = 'Month', y = 'Sea ice extent') +
  transition_reveal(Year)
```

![](03_OKINEDO_UZEZI_2020_files/figure-gfm/animated%20plot-1.gif)<!-- -->

### 3g. Extra Credit. Use the data and make something wholly new and awesome. Even extra extra credit for something amazing animated.

``` r
# I used the data to create a new plot called Awesome!
# The plot shows how sea ice extent transitioned over the years
awesome_plot <- ggplot(ice,
       aes(x = Month, y = Extent, size = Day, colour = Month_Name)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  # plot labels
  labs(title = "Transition of sea ice extent over time", x = "Month", y = "Extent")

awesome_plot +
  transition_time(Year) +
  labs(title = "Year: {frame_time}")
```

![](03_OKINEDO_UZEZI_2020_files/figure-gfm/an%20amazing%20plot-1.gif)<!-- -->
