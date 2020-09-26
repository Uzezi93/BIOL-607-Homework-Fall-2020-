---
title: "Sampling and iteration in Tidyverse"
author: "Uzezi Okinedo"
date: "9/18/2020"
output: html_document
  
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r libraries}
# Libraries for this assignment

library(dplyr)
library(purrr)
library(kableExtra)

```

# 1. Some data with Flippers

## 1a. Load the library palmerpenguins after installing it

```{r install palmerpenguins}

library(palmerpenguins)            # Load library palmerpenguins
```

## 1b. Show the head of the dataset penguins

```{r head of penguin dataset}

penguins %>%                       # penguins dataset
  head() %>%                       # head of penguins dataset
  kbl() %>%                        
  kable_styling()                  # Using KableExtra table formatting
```

## 1c. What do you learn by using str() and summary() on penguins()

```{r penguins summary and structure}

penguins %>%                         
  summary()                      # Summary of penguins dataset 
```

What the summary shows;

i) It shows the descriptive statistics of the dataset. This includes the min, max, mean, median, and quartiles of each column.

ii) It shows the names of each column in the dataset

iii) It also identifies columns with missing values(NA)

```{r penguins structure}

penguins %>%
  str()                           # structure of penguins dataset
```
What the structure explains;

i) It shows the dimensions of the dataset = 344 x 8

ii) It shows the class of each column. For example; 'species', 'island' and 'sex' are factors. 


## 1d. What are the quantiles of bill depth across the whole data set? What do those quantiles mean?

```{r quantiles of bill depth across whole data set}

penguins %>%                      # load penguins data
  select(bill_depth_mm) %>%       # select the bill depth column using the select() from dplyr.
  quantile(na.rm = TRUE)          # calculate the quantiles using the quantile()
```
The quantiles divide the distribution of bill_depth_mm into equal groups. 50% shows the median while 75% and 100% shows the median of the lower and upper half of the data.


# 2. DIPLYR

## 2a. If I have a vector, c(1,4,7,NA,9), what is its mean? Note, the NA is going to cause a problem. Look at ?mean to learn how to solve it.

```{r to calculate the mean of a vector}

c(1,4,7,NA,9) %>%                 # create a vector
mean(na.rm = TRUE)                # calculate the mean using the mean() and setting na.rm = TRUE to remove missing values
```


## 2b. What is the mean, sd, and median of body mass across the data set? Note, these NAs are going to cause some problems, so you might need to look at the documentation for the relevant functions.

```{r To calculate the mean, sd, and median of body mass across the data set }

penguins %>%                                         # load penguin data
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = TRUE),# Calculate the mean, sd and median of the body mass and summarize results into a data frame 
    sd_body_mass = sd(body_mass_g, na.rm = TRUE),
    med_body_mass = median(body_mass_g, na.rm = TRUE)) %>%
  kbl() %>%                        
  kable_styling()                  # Using KableExtra table formatting
```

## 2c. Repeat 2b, but, show us how these quantities differ by species

```{r to show how quantities differ by species}

penguins %>%
  group_by(species) %>% #group all species in the data using group_by function
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = TRUE),  # repeat 2b above
    sd_body_mass = sd(body_mass_g, na.rm = TRUE),
    med_body_mass = median(body_mass_g, na.rm = TRUE)) %>%
  kbl() %>%                        
  kable_styling()                  # Using KableExtra table formatting
```


## 2d. Repeat 2c, but just for Biscoe island. What is different in the results?

```{r To check for Biscoe Island}

penguins %>%
  filter(island == "Biscoe") %>% # filtered out Biscoe island using the filter()
  group_by(species, island) %>%  # Grouped species and island from dataset
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = TRUE),  # same as 2c above
    sd_body_mass = sd(body_mass_g, na.rm = TRUE),
    med_body_mass = median(body_mass_g, na.rm = TRUE)) %>%
kbl() %>%                        
  kable_styling()                  # Using KableExtra table formatting
```

The 'Chinstrap' specie is not included in the summarized dataframe. It can be inferred that the Chinstrap species is not found in Biscoe island.


## 2e. Make a species-island column in penguins using paste(). This is an awesome function that takes multiple strings, and slams them together using the argument sep = to define how the string should be combined.

```{r use paste() to make a species-island column in penguins}

penguins %>%    # pipe penguin data
group_by(species, island) %>% # group the species and island column together
mutate('species-island' = paste(as.character(species), as.character(island), sep = "_")) %>%  # create a new dataframe with 'species' island column using mutate() and paste(sep = "_")
  kbl() %>%                        
  kable_styling()                  # Using KableExtra table formatting

```


# 3. PLOTTING

## **Show the distribution of flipper_length_mm by species and island using boxplots. For one point of extra credit, redo creating the species_island column with the sep as \n instead of _. What does \n do? You will find it very handy in the future.**


```{r create a new penguins dataset (penguins_03)  using mutate and paste()}

penguins_03 <- penguins %>%     # Create a new dataframe called penguin_03 from Penguins data
  mutate('species-island' = paste("species", "island", sep = "\n"))                   # add a new species-island column using mutate() and paste(sep = "\n")

penguins_03                     # Show penguins_03 dataframe

```

```{r Show the distribution of flipper_length_mm by species and island using boxplots}

penguins_03 %>%                   # pipe penguin_03 data
  group_by(flipper_length_mm, `species-island`) %>%   # group flipper length and 'species-island' column
  summarise(flipper_length_mm = flipper_length_mm, `species-island`) %>% # create a dataframe of flipper length and species island using summarise()

  boxplot(flipper_length_mm ~ `species-island`, data = .) # make a boxplot 
```
The "\n" function separates pasted character with a new line.


# 3b. Show the relationship between average flipper length and average body mass by species and island. What do you see?

```{r relationship between average flipper length and average body mass by species and island using plot}

penguins_03 %>%       # pipe penguin_03 data 
  group_by(flipper_length_mm, body_mass_g, species-island) %>%  # group flipper length, body mass and species-island column
  summarise(avg_flipper_length_mm = mean(flipper_length_mm, na.rm = TRUE), avg_body_mass_g = mean(body_mass_g, na.rm = TRUE), `species-island`) %>% # create a dataframe that summarizes drouped elements
  
  plot(avg_flipper_length_mm ~ avg_body_mass_g, data = .) # plot average flipper length against average body mass

```

From the plot, it looks like there is a correlation between average flipper length and average body mass. That is, there is a linear relationship between average flipper length and average body mass.


# 3c. Interesting. What if you had made the same plot with the whole dataset? What do you see? Is there anything that could clarify the result any more? Think about it - lots of possible right answers here.

```{r plotting the whole dataset}

penguins %>%        # pipe penguin data
  mutate(avg_flipper_length_mm = mean(flipper_length_mm, na.rm = TRUE), avg_body_mass_g = mean(body_mass_g, na.rm = TRUE)) %>% # create a new column of average flipper length and average body mass in the penguins data set using mutate()
  
  plot(avg_flipper_length_mm ~ avg_body_mass_g, data = .) # plot average flipper length against average body mass using the whole dataset
 
```
My thoughts about this plot;

i) The average body mass across the dataset falls between 4000g to 4500g.

ii) The average flipper length is about 200mm 

iii) Values above the ranges mentioned above are probably outliers

iv) This plot correlates with the previous plot of average flipper length against body mass across species and island.



## 4. Simulation

# **4a.Grab the values for bill_length_mm for Gentoo penguins in Biscoe Island and put it into an object. Note, the dplyr function pull() is kinda cool, as if you apply it to a data frame, it will pull out a vector from a column of interest.** 

``` {r to grab values for all bill length}

bill_gent_bis <- penguins %>% 
  group_by(species, island) %>% # create a new dataframe with values for bill length for Gentoo species in Biscoe island
  filter(species == "Gentoo" | island == "Biscoe") %>%  # Filter Gentoo species and Biscoe island
  pull(bill_length_mm) # Use pull() to select bill length

bill_gent_bis


```

# 4b.Use replicate() to calculate the standard error of the mean 10 times. Use a formula! Don’t forget that NA values shouldn’t be included!

```{r to calculate the standard error of the mean 10 times}

as.data.frame(bill_gent_bis) %>%  # covert bill_gent_bis containing bill length of Gentoo species in Biscoe island 
  summarise(sd = sd(bill_gent_bis, na.rm = TRUE), n = n(),  se = replicate(n =10, sd/sqrt(n))) %>%  # calculate the standard deviations, replicate standard error 10x and summarize
  na.omit() %>%  # omit missing values
kbl() %>%                        
  kable_styling()                  # Using KableExtra table formatting
```

# 4c.Use map_df() to create a data frame with the mean and sd of different sample sizes using the first 5 through 100 values (so, n = 5:100 - smallest sample size will have the values 1-5). Make sure the sample size is included in the final data frame.

```{r create a data frame with mean and sd of different sample sizes}
library(purrr)

my_vec <- 1:100      # create a vector containing number 1 through 100

my_vec_data <- map_df(5:100, ~data.frame(m = mean(my_vec[1:.x]),
                         sd = sd(my_vec[1:.x])),
       .id = "sample_size")    # use map_df to create a data frame from the created vector. 
  

my_vec_data      # view new data frame

```

# 4d. Plot the relationship between sample size and SD and sample size versus SE of the mean. What difference do you see and why? Note, you’ll need to create a column for SE here!

# +2 EC for using par() to make a two-panel plot. Don’t forget to reset back to a single plot per panel after making a two-panel plot. Otherwise things get weird.


```{r relationship between SD and mean of sample sizes}

my_vec_data_02 <- my_vec_data %>%         # pipe data frame containing mean and SD of samples
  group_by(sample_size, sd) %>%    # group sample size and SD
  summarise(sample_size, sd, n = n(), se = sd/sqrt(n))    # calculate standard error and summarize sample_size, standard deviation and Standard error into one data frame

                 
my_vec_data_02     # summarized data showing se column

```

```{r plot standard deviation against standard error}
  plot(sd ~ se, data = my_vec_data_02)     # plot standard deviation against standard error
```
There's no difference between the SD and SE. That is; SD = SE


# +2 using par() for multiple plots

I plotted the standard Errors using plot() and hist() side by side

```{r Two panel plot}

par(mfrow = c(2, 2))     # setting the two panel plot
plot(my_vec_data_02$se)  # plot se from my_vec_data_02
hist(my_vec_data_02$se)  # make a histogram to show the distrubution of se from my_vec data
par(mfrow = c(1, 1))     # undo two-panel plot
```
  




  

