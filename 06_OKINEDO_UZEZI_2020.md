Inference and Likelihood
================
Uzezi Okinedo
10/20/2020

``` r
# load all required libraries
library(tidyr)
library(bbmle)
```

    ## Warning: package 'bbmle' was built under R version 4.0.3

    ## Loading required package: stats4

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:bbmle':
    ## 
    ##     slice

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(broom)
library(ggplot2)
library(rayshader)
```

    ## Warning: package 'rayshader' was built under R version 4.0.3

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(profileModel)
```

    ## Warning: package 'profileModel' was built under R version 4.0.3

**0.** Go through the faded examples in the lab. You don’t need to put
the output here - just, do them\! And let us know if you have any
remaining questions, or you feel comfortable. Full credit\!

1)  What does the qplot of the fitted and residual values explain? Won’t
    it make more sense to separate the residual from the fitted values
    with colors or something to easily assess the disparities that might
    exist?

2)  For making grids, what criteria are used in selecting numbers for
    crossing?

3)  What is the “length.out” argument used for? How is it different from
    the “by” argument?

4)  How is the response variable determined? I’m still not sure if my
    approach for determining this is correct.

**1.** Would you say you naturally gravitate towards deductive or
inductive inference? Why?

I naturally gravitate towards inductive inference because I am inclined
to observing patterns of occurrence before assuming any proposed
explanation or hypothesis.Furthermore, my explanations must be validated
through experiments before coming to a conclusion or before postulating
a theorem.

**2.** We talked about strictly interpreted Popperian Falsification
versus Lakatos’s view of a research program this week.

**2a.** Do you more strongly identify with one of these paradigms? Why?
+1 EC for direct quotes (if you want to do some additional reading)

“…in science, a ‘theory’ is really a succession of slightly different
theories and experimental techniques developed over time that all share
a common hard core…” -Imre Lakatos.

I strongly agree and identify with this paradigm of **Imre Lakatos** in
the sense that I view scientific knowledge as being progressive centered
around a base knowledge or “common hardcore”. Theories should not be
totally discarded but worked on through a research program leading to
the accumulation of novel knowledge and improved experimental
techniques.

**2b.** How does your own research program fit into one of these
paradigms?

My research program best aligns with Imre Lakatos’ paradigm of a
research program as being progressive or degenerative over time with a
central hardcore.

For my undergrad and Masters program, I worked on genetic diversity
assessment of some plant species using less novel techniques that
involved the use of Random Amplified Polymorphic DNA (RAPD) and Simple
Sequence Repeats (SSR). Although, the results obtained from this
research suggested significant levels of genetic diversity in these
species, it was limited in revealing the depth of diversity and the
relationship with adaption and selection.

Here at UMass Boston, my research still centers on “genetic diversity”
which is my “common hardcore” with respect to adaptation and selection
in plant species. The only difference is that my experimental approaches
have become more sophisticated to include techniques like GBS, RADSeq
and RNA-seq with the addition of better computational analysis tools
like R and Python.

**EC x4 2c.** This has been a shallow dive into Lakatos and Popper. Look
them or other philosophers of science and inference up - Kuhn,
Feyerabend, Musgrave, Deb Mayo, Sabra, Fillies, and others. What’s their
core idea, and why do you agree or disagree?

1)  **Popper**: Popper idealized science as progressing through a
    process of **falsification** and that theories whose predictions
    conflicted with experimental observation are discarded immediately.
    Therefore, he describes scientific progress as a process of
    **elimination**.

I strongly disagree with this paradigm because science theories should
be developed through further experiments or research and not eliminated.

2)  **Kuhn**: Kuhn postulated that science consisted of periods. He
    described a period called ‘**normal science**’, in which experiment
    and theory are performed within a particular paradigm, where
    scientists hold on to their theories despite anomalies. Very often,
    the trending paradigm is overturned, but even when such a shift
    happens, it does not rely on logic alone because observation is
    influenced by the paradigm in which it happens.

I strongly disagree with this paradigm because science should not be
influenced by societal or cultural trends and results should not be
manipulated to fit any generally accepted ideology or norm.

3)  **Feyerabend**: He developed the *anarchistic philosophy of
    science*. Feyerabend concludes that the progress of science cannot
    be ascertained in terms of one set of methodological rules that is
    always used by scientists; such a ”scientific method’ would in fact
    limit the activities of scientists and hamper scientific progress.

I strongly agree with the ideology of Feyerabend because he views
scientific progress from the perspective of both Lakatos and Kuhn. As
much the progress of science relies heavily on a research program, it is
not totally excluded from societal influence.

**\#\#Puffers\!\#\#**

**3.** Grid Sampling\! Based on Friday’s lab, load up the pufferfish
data and use grid sampling to find the MLE of the slope, intercept and
residual SD of this model. Feel free to eyeball results from an lm() fit
to get reasonable values. Try not to do this for a grid of more than
\~100K points (more if you want\!). It’s ok to be coarse. Compare to lm.

``` r
# download pufferfish data
download.file(url = "http://biol607.github.io/homework/data/16q11PufferfishMimicry%20Caley%20&%20Schluter%202003.csv",
              destfile = "raw_data/pufferfish.csv")
```

``` r
# read in pufferfish data and assign object
pufferfish <- read.csv("./raw_data/pufferfish.csv")

# view data
pufferfish
```

    ##    resemblance predators
    ## 1            1         2
    ## 2            1         6
    ## 3            1         5
    ## 4            1         5
    ## 5            1         0
    ## 6            1         3
    ## 7            1        11
    ## 8            2         6
    ## 9            2         8
    ## 10           2        11
    ## 11           2         7
    ## 12           3        11
    ## 13           3         9
    ## 14           3        15
    ## 15           3        15
    ## 16           4        11
    ## 17           4        11
    ## 18           4        11
    ## 19           4        18
    ## 20           4        14

``` r
pufferfish_lm = lm(predators ~ resemblance, data = pufferfish)

# Get coefficient estimates
tidy(pufferfish_lm)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.92     1.51       1.28 0.218    
    ## 2 resemblance     2.99     0.571      5.23 0.0000564

``` r
# Get estimate for RMSE
glance(pufferfish_lm)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.603         0.581  3.05      27.4 5.64e-5     1  -49.6  105.  108.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
# slope(b0) = 3.0
# intercept(b1) = 2.0
# SD(sigma) = 3.1

# A function, given a set of values, returns the log likelihood
norm_loglik = function(b0, b1, sigma) { 

  # Compute yhats and residuals
  fit = b0 + b1 * pufferfish$resemblance
  error = pufferfish$predators - fit
  
  # Compute the log-likelihood
  log_lik = sum(dnorm(error, fit, sd = sigma, log = TRUE))
  
  
  return(log_lik)
  
} 

# Use crossing to generate testing parameters
pufferfish_norm <- crossing(b0 = seq(0.1, 2.0, by= 0.1),
                            b1 = seq(0.5, 2.5, by = 0.1),
                            sigma = seq(1.1, 3.1, by = 0.1)) %>%
  rowwise() %>%
  mutate(log_lik = norm_loglik(b0, b1, sigma)) %>%
  ungroup()

# Get MLE
pufferfish_norm %>%
  filter(log_lik == max(log_lik))
```

    ## # A tibble: 2 x 4
    ##      b0    b1 sigma log_lik
    ##   <dbl> <dbl> <dbl>   <dbl>
    ## 1   0.9   1.5   2.9   -49.7
    ## 2   1     1.5   2.9   -49.7

The log likelihood obtained from the lm analysis is -49.64899 which is
pretty close to the log likelihood value (-49.6492) obtained using grid
crossing.

**4.** Surfaces\! Filter the dataset to the MLE of the SD. Plot the
surface for the slope and intercept in whatever way you find most
compelling. You might want to play around with zooming in to different
regions, etc. Have fun\!

``` r
# visualize with contour plot!
ggplot(data = pufferfish_norm %>% filter(log_lik > max(log_lik) - 3),
       mapping = aes(x = b0, y = b1, z = log_lik)) +
  geom_contour_filled(bins = 20) 
```

![](06_OKINEDO_UZEZI_2020_files/figure-gfm/plot%20surface%20for%20MLE%20of%20SD-1.png)<!-- -->

**5.** GLM\! Now, compare those results to results from glm. Show the
profiles and confidence intervals from glm() for the slope and
intercept. Also show how you validate assumptions.

``` r
#using glm to show profiles
pufferfish_mle <- glm(predators ~ resemblance,
                data = pufferfish,
                family = gaussian(link = "identity"))

#view pufferfish_mle
pufferfish_mle
```

    ## 
    ## Call:  glm(formula = predators ~ resemblance, family = gaussian(link = "identity"), 
    ##     data = pufferfish)
    ## 
    ## Coefficients:
    ## (Intercept)  resemblance  
    ##       1.925        2.989  
    ## 
    ## Degrees of Freedom: 19 Total (i.e. Null);  18 Residual
    ## Null Deviance:       423 
    ## Residual Deviance: 167.8     AIC: 105.3

``` r
# show profiles with CI from glm
prof <- profileModel(pufferfish_mle,
                     objective = "ordinaryDeviance",
                     quantile = qchisq(0.95, 1))
```

    ## Preliminary iteration .. Done
    ## 
    ## Profiling for parameter (Intercept) ... Done
    ## Profiling for parameter resemblance ... Done

``` r
#plot profiles with grid points
plot(prof, print.grid.points = TRUE)
```

![](06_OKINEDO_UZEZI_2020_files/figure-gfm/compare%20results%20to%20GLM-1.png)<!-- -->

``` r
#validate assumptions
#Tau test using the signed square root of the deviance
prof_mass <- profile(pufferfish_mle)
plot(prof_mass)
```

![](06_OKINEDO_UZEZI_2020_files/figure-gfm/validate%20assumptions-1.png)<!-- -->
The profile function is used to test the validity of a profile. This
test produces a straight line from the parabola making visualization of
deviance much more possible, hence it is a good profile.

**EC 6.** Get Outside of GLM\! So, often, we have more complex models
than the above. There are a variety of optimizers out there, and
packages for accessing them. One of the best is bbmle by Ecologist Ben
Bolker (whose dad is emeritus at UMB in computer science\! Go visit
him\! He’s fantastic\!)

Load up ’bbmle and try out mle2. It’s a bit different, in that the first
argument is a function that minimizes the log likelihood (not
maximizes). The second argument is a list of start values -
e.g. list(slope = 2, intercept = 5, resid\_sd = 2). Try and fit your
model with mle2 using start values close to the actual estimates. Look
at the summary and plot the profile. Note, you might get a lot of errors
because it will try impossible values of your residual SD. Also, note
that you’ll have to rewrite your likelihood function to return the
negative log likelihood (or write a wrapper that does so). A small thing

``` r
# rewrite likelihood function to return the negative log likelihood
neg_log_lik = function(slope, intercept, resid_sd) { 

  # Compute fit
  fit = slope + intercept * pufferfish$resemblance
  error = pufferfish$predators - fit
  
  # Compute the negative log-likelihood
  neg_log_L = -sum(error, fit, sd = resid_sd, log = TRUE)
  
  return(neg_log_L)
  
} 

# test function
neg_log_lik(2, 5, 2)
```

    ## [1] -182

**EC 6a.** Start values\! What happens if you start with start values
very far away from the initial values. Failing here is fine. But what do
you think is happening, and what does this say about the value of start
values?

``` r
# Fit model using MLE
mle_results = mle2(minuslogl = neg_log_lik, start = list(slope = 50, intercept = 100, resid_sd = 50))

# View results
summary(mle_results)
```

    ## Maximum likelihood estimation
    ## 
    ## Call:
    ## mle2(minuslogl = neg_log_lik, start = list(slope = 50, intercept = 100, 
    ##     resid_sd = 50))
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value  Pr(z)
    ## slope     5.0000e+01 1.4525e+04  0.0034 0.9973
    ## intercept 1.0000e+02 7.2623e+03  0.0138 0.9890
    ## resid_sd  7.0369e+10 9.1788e+11  0.0767 0.9389
    ## 
    ## -2 log L: -140737488869

``` r
# check profile
profile(mle_results)
```

    ## Profiling has found a better solution,so original fit had not converged:

    ## (new deviance=-1.407e+11, old deviance=-1.407e+11, diff=-2.014)

    ## Returning better fit ...

    ## 
    ## Call:
    ## mle2(minuslogl = function(slope, intercept, resid_sd) { 
    ## 
    ##   # Compute fit
    ##   fit = slope + intercept * pufferfish$resemblance
    ##   error = pufferfish$predators - fit
    ##   
    ##   # Compute the negative log-likelihood
    ##   neg_log_L = -sum(error, fit, sd = resid_sd, log = TRUE)
    ##   
    ##   return(neg_log_L)
    ##   
    ## }, start = list(slope = 50, intercept = 100, resid_sd = 70368744254.3431), 
    ##     fixed = list(slope = c(slope = -10358.4822838702)), skip.hessian = TRUE, 
    ##     lower = -Inf, upper = Inf, control = list())
    ## 
    ## Coefficients:
    ##   slope.slope     intercept      resid_sd 
    ## -1.035848e+04  1.000000e+02  7.036874e+10 
    ## 
    ## Log-likelihood: 70368744435

It seems these start values are not suitable for fitting this model as
there was no convergence in the fit.

**EC 6b** Algorithms\! By default, mle2 uses the Nelder-Mead algorithm
via the optim function. What happens if you add a method argument to
“SANN” or “L-BFGS-B” (and for the later, which is bounded sampling,
give it a lower argument for your residual value, so it’s always
positive). See ?optim for some more guidance. Do these both converge to
the same value? Based on their profiles, do you trust them? (Note,
Simulated annealing takes a looooong time. Go have a cuppa while the
profile for that one runs).

``` r
params <- list()    # set up empty list to store parameters
params$slope=1          # fill the list with the "best fit" parameter set from above (this is still just an educated guess)   
params$intercept=2 
params$resid_sd=3.5

# view parameters
params
```

    ## $slope
    ## [1] 1
    ## 
    ## $intercept
    ## [1] 2
    ## 
    ## $resid_sd
    ## [1] 3.5

``` r
# create a function for deterministic exponential decline (assuming slope is negative)
Deterministic_component <- function(xvals, slope, intercept){
  yexp <- slope*exp(intercept*xvals)        
  return(yexp)
}


# rewrite likelihood function to return the negative log likelihood using the deterministic_component function created above
neg_log_lik_02 <- function(params,df,yvar,xvar){
  
  neg_log <- -sum(dnorm(df[,yvar],Deterministic_component(df[,xvar],params['slope'],params['intercept']),sqrt(params['resid_sd']),log=TRUE))
  return(neg_log)
}
neg_log_lik_02(unlist(params),df=pufferfish,yvar="predators",xvar="resemblance")
```

    ## [1] 6380612

``` r
# use optim to optimize function and set method = "SANN"
MLE_01 <- optim(fn=neg_log_lik_02,par=unlist(params),df=pufferfish,yvar="predators",xvar="resemblance",control=list(fnscale=-1), method = "SANN") 

# view optimized parameters
MLE_01$par
```

    ##     slope intercept  resid_sd 
    ##  3.197435 88.516833  4.919651

``` r
# use optim to optimize function and set method = "L-BFGS-B"
MLE_02 <- optim(fn=neg_log_lik_02,par=unlist(params),df=pufferfish,yvar="predators",xvar="resemblance",control=list(fnscale=-1),lower=c(0.1,0.5), upper=c(2.5,3.5), hessian = TRUE, method = "L-BFGS-B") 

# view parameters from this optimization
MLE_02$par
```

    ##     slope intercept  resid_sd 
    ##       2.5       3.5       0.1

These two methods “SANN” and “L-BFGS-B” do not converge at the same
values.

These methods cannot be trusted because the “SANN” method gives values
that totally vary from any expected value for the slope and intercept.
On the other hand, the “L-BFGS-B” gives values that are only within the
box-constraint of numbers provided as there’s a probability of having
values outside the specified numbers.
