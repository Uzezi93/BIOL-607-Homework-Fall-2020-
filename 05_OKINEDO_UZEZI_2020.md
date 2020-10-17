Correlation And Regression
================
Uzezi Okinedo
10/14/2020

``` r
library(ggplot2)
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
library(tidyr)
library(broom)
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(ggfortify)
```

``` r
# download all W&S data
download.file(url = "https://whitlockschluter.zoology.ubc.ca/wp-content/data/ABD_all_data.zip",
              destfile = "raw_data/all_data.zip")
```

    ## Warning in download.file(url = "https://whitlockschluter.zoology.ubc.ca/wp-
    ## content/data/ABD_all_data.zip", : downloaded length 65536 != reported length
    ## 211212

**1. Correlation - W\&S Chapter 16**

``` r
# read data
michelli_data <- read.csv("./raw_data/all_data/chapter16/chap16q15LanguageGreyMatter.csv")

# view data
michelli_data
```

    ##    proficiency greymatter
    ## 1         0.26     -0.070
    ## 2         0.44     -0.080
    ## 3         0.89     -0.008
    ## 4         1.26     -0.009
    ## 5         1.69     -0.023
    ## 6         1.97     -0.009
    ## 7         1.98     -0.036
    ## 8         2.24     -0.029
    ## 9         2.24     -0.008
    ## 10        2.58     -0.023
    ## 11        2.50     -0.006
    ## 12        2.75     -0.008
    ## 13        3.25     -0.006
    ## 14        3.85      0.022
    ## 15        3.04      0.018
    ## 16        2.55      0.023
    ## 17        2.50      0.022
    ## 18        3.11      0.036
    ## 19        3.18      0.059
    ## 20        3.52      0.062
    ## 21        3.59      0.049
    ## 22        3.40      0.033

**a.** Display the association between the two variables in a scatter
plot.

``` r
# Basic scatter plot using ggplot2
ggplot(michelli_data, aes(x= proficiency, y= greymatter)) +
  geom_point()
```

![](05_OKINEDO_UZEZI_2020_files/figure-gfm/display%20association%20between%20the%20two%20variables-1.png)<!-- -->

**b.** Calculate the correlation between second language proficiency and
gray-matter density.

``` r
# use the cor() to calculate correlation
cor(michelli_data$proficiency, michelli_data$greymatter)
```

    ## [1] 0.8183134

The correlation between second language proficiency and grey matter
density is 0.8183134

**c.** Test the null hypothesis of zero correlation.

``` r
# Fit a model
michelli_lm <- lm(greymatter ~ proficiency, data = michelli_data)
# use anova to check p-value
anova(michelli_lm) %>%
  # use tidy() to summarize model statistical findings
  tidy()
```

    ## # A tibble: 2 x 6
    ##   term           df   sumsq   meansq statistic     p.value
    ##   <chr>       <int>   <dbl>    <dbl>     <dbl>       <dbl>
    ## 1 proficiency     1 0.0193  0.0193        40.5  0.00000326
    ## 2 Residuals      20 0.00953 0.000477      NA   NA

**d.** What are your assumptions in part (c)?

1)  The population correlation co-efficient (p) is not significantly
    different from zero. Therefore, a regression line cannot be used in
    modeling a relationship between second language proficiency and gray
    matter density.

**e.** Does the scatter plot support these assumptions? Explain.

Yes, the scatter plot explains these assumptions. The plot shows an
association between second language proficiency and gray matter density
but fails to show any linear relationship between both variables as data
points are not in a straight line along the plot.

**f.** Do the results demonstrate that second language proficiency
affects gray-matter density in the brain? Why or why not?

No, these results do not demonstrate if second language proficiency
affects gray-matter density because the population correlation
co-efficient or p-value is close to 0. Therefore, the sample data cannot
be used to infer any relationship.

**2. Correlation - W\&S Chapter 16**

``` r
liver_data <- read.csv("./raw_data/all_data/chapter16/chap16q19LiverPreparation.csv")

liver_data
```

    ##   concentration unboundFraction
    ## 1           2.8            0.63
    ## 2           5.8            0.44
    ## 3          12.0            0.31
    ## 4          23.9            0.19
    ## 5          47.8            0.13

**a.** Calculate the correlation coefficient between the taurocholate
unbound fraction and the concentration.

``` r
cor(liver_data$concentration, liver_data$unboundFraction)
```

    ## [1] -0.8563765

**b.** Plot the relationship between the two variables in a graph.

``` r
# Basic scatter plot using ggplot2
ggplot(liver_data, aes(x= concentration, y= unboundFraction)) +
  geom_point()
```

![](05_OKINEDO_UZEZI_2020_files/figure-gfm/plot%20the%20relationship%20between%20the%20two%20variables-1.png)<!-- -->

**c.** Examine the plot in part (b). The relationship appears to be
maximally strong, yet the correlation coefficient you calculated in part
(a) is not near the maximum possible value. Why not?

The variables might have been log transformed which explains the
disparity between the plot and the calculated correlation.

**d.** What steps would you take with these data to meet the assumptions
of correlation analysis?

Nonparametric Spearman’s rank correlation can be used as a test of zero
correlation between variables that do not meet the assumption of
bivariate normality, even after data transformation.

**3. Correlation SE**

Consider the following dataset:

``` r
cat <- c(-0.30, 0.42, 0.85, -0.45, 0.22, -0.12, 1.46, -0.79, 0.40, -0.07)
happiness_score <- c(-0.57, -0.10, -0.04, -0.29, 0.42, -0.92, 0.99, -0.62, 1.14, 0.33)

data_set <- data.frame(cat, happiness_score)

data_set
```

    ##      cat happiness_score
    ## 1  -0.30           -0.57
    ## 2   0.42           -0.10
    ## 3   0.85           -0.04
    ## 4  -0.45           -0.29
    ## 5   0.22            0.42
    ## 6  -0.12           -0.92
    ## 7   1.46            0.99
    ## 8  -0.79           -0.62
    ## 9   0.40            1.14
    ## 10 -0.07            0.33

**3a.** Are these two variables correlated? What is the output of cor()
here. What does a test show you?

``` r
# Basic scatter plot using ggplot2 to check the association between the variables
ggplot(data_set, aes(x= cat, y= happiness_score)) +
  geom_point()
```

![](05_OKINEDO_UZEZI_2020_files/figure-gfm/check%20the%20correlation%20of%20the%20values-1.png)<!-- -->

``` r
# using the cor() function to calculate the correlation
cor(data_set$cat, data_set$happiness_score)
```

    ## [1] 0.6758738

``` r
# testing correlation using Anova
data_set_lm <- lm(happiness_score ~ cat, data =data_set)

anova(data_set_lm) %>%
  tidy()
```

    ## # A tibble: 2 x 6
    ##   term         df sumsq meansq statistic p.value
    ##   <chr>     <int> <dbl>  <dbl>     <dbl>   <dbl>
    ## 1 cat           1  1.92  1.92       6.73  0.0319
    ## 2 Residuals     8  2.28  0.286     NA    NA

\-From the scatter plot, these values do not show correlation. - The
value obtained by using the cor() is 0.6758738 - The Anova test shows a
p-value \< 0.05 which confirms no correlation as a regression line
cannot be used to model any relationship between the two variables.

**3b.** What is the SE of the correlation based on the info from
cor.test()

``` r
# create a function that calculates the standard error from the info of cor.test
cor.test.plus <- function(x) {
  list(x, 
       Standard.Error = unname(sqrt((1 - x$estimate^2)/x$parameter)))
}

# use function to determine SE
cor.test.plus(cor.test(data_set$cat, data_set$happiness_score))
```

    ## [[1]]
    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  data_set$cat and data_set$happiness_score
    ## t = 2.5938, df = 8, p-value = 0.03193
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.08050709 0.91578829
    ## sample estimates:
    ##       cor 
    ## 0.6758738 
    ## 
    ## 
    ## $Standard.Error
    ## [1] 0.260575

The standard error is 0.260575

**3c.**Now, what is the SE via simulation? To do this, you’ll need to
use cor() and get the relevant parameter from the output (remember - you
get a matrix back, so, what’s the right index\!), replicate(), and
sample() or dplyr::sample\_n() with replace=TRUE to get, let’s say, 1000
correlations. How does this compare to your value above?

``` r
# create an object for correlation values of the data_set
cor_data <- cor(data_set)
# view parameters in the created object
cor_data
```

    ##                       cat happiness_score
    ## cat             1.0000000       0.6758738
    ## happiness_score 0.6758738       1.0000000

``` r
# list parameters;
# N = number of replications or simulations
N = 1000
# r = correlation value
r = 0.6758738
# n = length of dataframe
n = 10

# use the replicate function to simulate SE 1000x 
replicate(N, cor(mvrnorm(10, c(0,0), cor_data))[2,1], sqrt((1-r^2)/(n-2)))
```

    ##    [1]  0.626110976  0.653582605  0.891149436  0.840541581  0.844757367
    ##    [6]  0.735746357  0.672762207  0.786440958  0.709663439  0.757517946
    ##   [11]  0.756652556  0.394053864  0.565869919  0.592131697  0.675347835
    ##   [16]  0.652674682  0.448609743  0.766553244  0.846537415  0.757392021
    ##   [21]  0.467895799  0.729274727  0.221120704  0.840114475  0.535978040
    ##   [26]  0.830448315  0.872904878  0.828943476  0.905034627  0.754542710
    ##   [31]  0.831248396  0.888677647  0.475712417  0.800272340  0.488447221
    ##   [36]  0.861167532  0.838345780  0.436406539  0.918362839  0.755353904
    ##   [41]  0.846766812  0.598143265  0.869334665  0.829650958  0.682043837
    ##   [46]  0.763212141  0.461557552  0.692337073  0.754335313  0.630237759
    ##   [51]  0.547145800  0.635963192  0.735928396  0.739161594  0.216666001
    ##   [56]  0.610044159  0.821190148  0.496157598  0.532806975  0.572072696
    ##   [61]  0.611571469  0.676317812  0.772723947  0.352593294  0.815346162
    ##   [66]  0.493157987  0.868406454  0.839972070 -0.101198141  0.653461818
    ##   [71]  0.759419373  0.856783004  0.725779668  0.366807192  0.795224813
    ##   [76]  0.835163005  0.800888403  0.736230433  0.802230053  0.609957811
    ##   [81]  0.825936880  0.815462833  0.819927941  0.647630549  0.409732150
    ##   [86]  0.879775878  0.754526110  0.650509832  0.781985042  0.895350604
    ##   [91]  0.352512000  0.624065379  0.840082762  0.812754987  0.841682951
    ##   [96]  0.650441371  0.939952316  0.859766865  0.406991473  0.900518393
    ##  [101]  0.780558997  0.709772875  0.944656143  0.375407910  0.762828564
    ##  [106]  0.657132012  0.447104711  0.356669609  0.369527881  0.709216008
    ##  [111]  0.803039091  0.762924435  0.849088118  0.665758062  0.852740741
    ##  [116]  0.653643287  0.517057567  0.584343576  0.893251863  0.773615146
    ##  [121]  0.859235309  0.541883853  0.483116737  0.306564756  0.531275201
    ##  [126]  0.525428702  0.626055713 -0.012141884  0.606887252  0.403994954
    ##  [131]  0.187487410  0.758138865  0.865409754  0.796769547  0.757061917
    ##  [136]  0.841715009  0.600584362  0.269931604  0.724018229  0.837811898
    ##  [141]  0.599312211  0.858794517  0.591348786  0.233572100  0.804939536
    ##  [146]  0.837998232  0.767657706  0.845971071  0.759409451  0.807505602
    ##  [151]  0.604110586  0.524591432  0.525988718  0.825904947  0.744431127
    ##  [156]  0.876738110  0.755917936  0.297697634  0.469623912  0.695612889
    ##  [161]  0.493652072  0.782415196  0.931853616  0.892909168  0.399971093
    ##  [166]  0.753256282  0.736254595  0.522364012  0.659457579  0.764923282
    ##  [171]  0.539624106  0.654146865  0.737833837  0.633518612  0.566668797
    ##  [176]  0.402995163  0.841941125  0.826075345  0.582388595  0.283349442
    ##  [181]  0.444426370  0.414105074  0.365709101  0.404208087  0.637824533
    ##  [186]  0.815205229  0.676724993  0.777080503  0.926333512  0.695389265
    ##  [191]  0.711258938  0.795740600  0.500467215  0.310951053  0.652471118
    ##  [196]  0.751455344  0.666250263  0.813489007  0.498530025  0.158110934
    ##  [201]  0.973414252  0.657687183  0.807999024  0.817102911  0.804377220
    ##  [206]  0.737970338  0.616780275  0.905706379  0.915647761  0.639767684
    ##  [211]  0.591228442  0.659074801  0.635725007  0.200286373  0.380121103
    ##  [216]  0.815572473  0.762722584  0.566730439  0.346366657  0.499791644
    ##  [221]  0.645092994  0.862701760 -0.073934025  0.850110823  0.663360142
    ##  [226]  0.621202189  0.876901054  0.772247463  0.498516104  0.800748331
    ##  [231]  0.381946545  0.876720026  0.719975869  0.722688921  0.782994462
    ##  [236]  0.695634987  0.534776574  0.804697856  0.888273331  0.772306053
    ##  [241]  0.614641834  0.864725589  0.845902835  0.831253096  0.459919535
    ##  [246]  0.880160932  0.451822238  0.731688710  0.178170808  0.675071616
    ##  [251]  0.785103416  0.641547543  0.808221704  0.865706534  0.647600280
    ##  [256]  0.911068761  0.645605690  0.729698852  0.878920520  0.302727943
    ##  [261]  0.796259626  0.717252252  0.506005596  0.795945713  0.851227083
    ##  [266]  0.908160409  0.280092985  0.207130578  0.942988710  0.759940994
    ##  [271]  0.750362033  0.808168926  0.497034871  0.474574515  0.383280991
    ##  [276]  0.933822330  0.874951848  0.832418897  0.904961820  0.835156203
    ##  [281]  0.823981593  0.828580251  0.419027981  0.604447630  0.631654492
    ##  [286]  0.749179057  0.638406887  0.640963031  0.811081595  0.380172565
    ##  [291]  0.796187880  0.581891266  0.781986997  0.556159245  0.332893265
    ##  [296]  0.621559838  0.671308914  0.740893226  0.589140977  0.636977938
    ##  [301]  0.645102801  0.780815686  0.543053124  0.599830099  0.490060804
    ##  [306]  0.729407988  0.682996851  0.880617570  0.684933804  0.548574071
    ##  [311]  0.596186096  0.644102043  0.737778771  0.395151753  0.854112895
    ##  [316]  0.612090123  0.740330885  0.478049908  0.868898433  0.487760976
    ##  [321]  0.720749798  0.868182729  0.378817233  0.831535638  0.528097067
    ##  [326]  0.321646383  0.847105928  0.509558230  0.475415286  0.526294400
    ##  [331]  0.744605658  0.433612544  0.374489191  0.546298954  0.354895563
    ##  [336]  0.632222104  0.583132433  0.684087463  0.761350431  0.743679442
    ##  [341]  0.877667273  0.496130554  0.731275400  0.405213558  0.309650779
    ##  [346]  0.665863015  0.729632852  0.830981368  0.883724129  0.672933884
    ##  [351]  0.919689570  0.500051797  0.466430367  0.740431665  0.355416425
    ##  [356]  0.569695746  0.763513337  0.866980386  0.763140203  0.693791959
    ##  [361]  0.765356077  0.683951080  0.821422838  0.602570609  0.657129081
    ##  [366]  0.426555663  0.610763976  0.930595617  0.661493626  0.402022625
    ##  [371]  0.725701503  0.318271547  0.433442522  0.851665358  0.590471933
    ##  [376]  0.894655105  0.806905276  0.594168339  0.741824194  0.534166101
    ##  [381]  0.550814236  0.890625816  0.612741721  0.711298608  0.550738150
    ##  [386]  0.763399602  0.525568103  0.785975517  0.592044643  0.757485827
    ##  [391]  0.637993621 -0.026641235  0.809223747  0.853231579  0.858319191
    ##  [396]  0.855536235  0.684210635  0.857746672  0.786657844  0.826348453
    ##  [401]  0.861992540  0.381196515  0.866328086  0.752726631  0.851602915
    ##  [406]  0.923406197  0.409110086  0.844493920  0.111673267  0.327176510
    ##  [411]  0.517338961  0.755584238  0.321076934  0.766875536  0.806984121
    ##  [416]  0.751845683  0.158292418  0.696205593  0.844448426  0.346887031
    ##  [421]  0.875467443  0.753358578  0.864737720  0.812122008  0.508818298
    ##  [426]  0.327980655  0.867312899  0.934959290  0.878756876  0.951647494
    ##  [431]  0.315416853  0.852384418  0.831246309  0.649093973  0.929427454
    ##  [436]  0.522048909  0.598262119 -0.002179702  0.925067675  0.756431914
    ##  [441]  0.778514762  0.312140004  0.746196539 -0.152974519  0.717957084
    ##  [446]  0.572310942  0.707948344  0.861040999  0.748183249  0.355812947
    ##  [451]  0.792360664  0.844095882  0.968430272  0.545277006  0.569220219
    ##  [456]  0.739345537  0.847479039  0.379592859  0.470398059  0.906426150
    ##  [461]  0.612774874  0.870961018  0.798701693  0.758049619  0.315333026
    ##  [466]  0.711185783  0.365234693  0.574814841  0.785084110  0.909431776
    ##  [471]  0.475223756  0.223937667  0.380296626  0.832416559  0.774643110
    ##  [476]  0.734706987  0.574736406  0.818710240  0.716862916  0.745872504
    ##  [481]  0.757415999  0.077169756  0.652425068  0.883494921  0.612711173
    ##  [486]  0.424095906  0.393122789  0.742858362  0.555151180  0.734628264
    ##  [491]  0.799061774  0.654981787  0.935958880  0.661279795  0.759807896
    ##  [496]  0.857188179  0.977985767  0.878116240  0.391998561  0.558848125
    ##  [501]  0.765749162 -0.094675118  0.872621470  0.643277462  0.738437705
    ##  [506]  0.567225753  0.913549222  0.818120374  0.844280799  0.507437830
    ##  [511]  0.662801717  0.810088753  0.408331629  0.798709844  0.771239461
    ##  [516]  0.321324493  0.790351029  0.237465851  0.652186031  0.523281604
    ##  [521]  0.653025749  0.146123903  0.491790866  0.730373565  0.835513390
    ##  [526]  0.872284812  0.675957677  0.407488915  0.701150700  0.656896796
    ##  [531]  0.764830339  0.769415215  0.352167610  0.766703252  0.629565721
    ##  [536]  0.660021491  0.751804055  0.691784143  0.872754409  0.900642247
    ##  [541]  0.892464783  0.488194337  0.924581217  0.600871094  0.522248063
    ##  [546]  0.080272573  0.894005390  0.622935756  0.846649777  0.738541533
    ##  [551]  0.745952724  0.742225432  0.610651247  0.562055295  0.541199715
    ##  [556]  0.938715141  0.137014567  0.540355441  0.535489908  0.567051849
    ##  [561]  0.915103999  0.656602738  0.741526081  0.843119200  0.518393274
    ##  [566]  0.840741282  0.533566287  0.485237483  0.751775409  0.750811282
    ##  [571]  0.862696509  0.361794897  0.821357257  0.755281880  0.716813808
    ##  [576]  0.788711039  0.660319906  0.701430977  0.637028502  0.865338937
    ##  [581]  0.875165277  0.560958466  0.785563223  0.852489845  0.462657135
    ##  [586]  0.857260703  0.650690938  0.539873674  0.846898001  0.423369363
    ##  [591]  0.656141349  0.391019345  0.481007625  0.596704178  0.680665382
    ##  [596]  0.663941533  0.633059472  0.580493264  0.387401819  0.376212782
    ##  [601]  0.278309184  0.452633348  0.797577116  0.738868546  0.859490019
    ##  [606]  0.451631090  0.665602333  0.879921747  0.485940423  0.758657619
    ##  [611]  0.794256567  0.769172673  0.870681107  0.662312130  0.685833295
    ##  [616]  0.737325408  0.715711983  0.358090991  0.794246836  0.449764666
    ##  [621]  0.613597249  0.534808043  0.737966103  0.804524956  0.521444648
    ##  [626]  0.756896298  0.380646436  0.584219268  0.537525144  0.124659048
    ##  [631]  0.588545977  0.683348941  0.683470109  0.882597812  0.744651406
    ##  [636]  0.369904622  0.645735499  0.232584324  0.453148418  0.557368623
    ##  [641]  0.661022428  0.839885724  0.822437068  0.557478248 -0.020489132
    ##  [646]  0.432855549  0.411808941  0.850960163  0.651383915  0.659408209
    ##  [651]  0.818682627  0.394759078  0.807925167  0.545396234  0.795518649
    ##  [656]  0.578410823  0.668846662  0.799218125  0.555629068  0.537750997
    ##  [661]  0.540666547  0.693538032  0.681655205  0.539286714  0.650463844
    ##  [666]  0.760341586  0.773498739  0.874385779  0.801813149  0.872158646
    ##  [671]  0.727291996  0.623446279  0.887602446  0.416117340  0.074097229
    ##  [676]  0.675473081  0.698064829  0.899054335  0.307731672  0.790407738
    ##  [681]  0.716526485  0.790020240  0.684195366  0.847418455  0.710572171
    ##  [686]  0.915319157  0.454410276  0.864524434  0.449165590  0.803489982
    ##  [691]  0.687539978  0.867033635  0.804775576  0.705069388  0.851661557
    ##  [696]  0.690774314  0.787338007  0.420381867  0.110759338 -0.005206072
    ##  [701]  0.909831217  0.781336598  0.710167486  0.895629975  0.867368766
    ##  [706]  0.798786696  0.224498437  0.781333837  0.914054686  0.870891201
    ##  [711]  0.466058866  0.760822156  0.621739799  0.761637243  0.758397463
    ##  [716]  0.652394680  0.858610949  0.375743337  0.696351699  0.861229264
    ##  [721]  0.873269408  0.825361855  0.525863282  0.684865881  0.919668209
    ##  [726]  0.640383611  0.911328026  0.896432764  0.641331229  0.476451292
    ##  [731]  0.575453653  0.532690874  0.542866077  0.579251006  0.696141404
    ##  [736]  0.842661761  0.825483532  0.911985787  0.636044069  0.598733131
    ##  [741]  0.745106995  0.377319912  0.156379606  0.401414929  0.817582134
    ##  [746]  0.427442596  0.723371777  0.872098956  0.698711186  0.391658286
    ##  [751]  0.757743676  0.601489311  0.943914712  0.894651250  0.623488660
    ##  [756]  0.765287235  0.896585351  0.669116137  0.398217111  0.762167580
    ##  [761]  0.753012897  0.706276385  0.476254016  0.356856539  0.771967927
    ##  [766]  0.729567251  0.039878029  0.452448476  0.726097616  0.377600711
    ##  [771]  0.855728292  0.525089594  0.778762794  0.916106183  0.434409058
    ##  [776]  0.837765207  0.692204804  0.690324491  0.866702877  0.707169120
    ##  [781]  0.452363115  0.872067630  0.789582581  0.828474659  0.755468427
    ##  [786]  0.813120498  0.550660763  0.871407447  0.911959285  0.413205700
    ##  [791]  0.502844171  0.796114726  0.662818963  0.712016277  0.668706109
    ##  [796]  0.695878385  0.844660962  0.752282803  0.391830073  0.619946817
    ##  [801]  0.667850635  0.708513217  0.687661627  0.590510860  0.361242822
    ##  [806]  0.687500411  0.461444648  0.589026047  0.863448405  0.725943926
    ##  [811]  0.875888562  0.795029759  0.877521138  0.846165370  0.126002911
    ##  [816]  0.793037159  0.507098890  0.838700051  0.701364979  0.893291453
    ##  [821]  0.838954354  0.808043702  0.671245843  0.712441562  0.633351655
    ##  [826]  0.310641055  0.803555521  0.621850577  0.742298978  0.648665576
    ##  [831]  0.711100472  0.633394988  0.879706816  0.184126896  0.337412816
    ##  [836]  0.480989473  0.467797816  0.645572045  0.762642658  0.903187818
    ##  [841]  0.791138011  0.610119604  0.932763316  0.510915529  0.459855339
    ##  [846]  0.689507503  0.417742699  0.769452702  0.555032405  0.722723894
    ##  [851]  0.510901183  0.761010989  0.889243065  0.707764702  0.630550995
    ##  [856]  0.572064798  0.643958511  0.679322754  0.333994239  0.559076426
    ##  [861]  0.305897160  0.791102900  0.877768741  0.200816147  0.432109281
    ##  [866]  0.889994361  0.804002761  0.711933631  0.828374119  0.621005620
    ##  [871]  0.624104463  0.777554961  0.700506930  0.702711760  0.789449398
    ##  [876]  0.539262947  0.856365077  0.605445874  0.602450971  0.412308512
    ##  [881]  0.505478618  0.875811773  0.661142035  0.623308068  0.839012595
    ##  [886]  0.390601207  0.402147716  0.710664152  0.537772936  0.694272374
    ##  [891]  0.489775831  0.636860453  0.748359628  0.619354565  0.975168944
    ##  [896]  0.836217943  0.646230856  0.613497263  0.765987950  0.571563274
    ##  [901]  0.421738842  0.565668732  0.898402192  0.945858766  0.434135556
    ##  [906]  0.722171269  0.826541333  0.896286075  0.690154346  0.596677106
    ##  [911]  0.521822111  0.793740865  0.453074759  0.437290550  0.646064527
    ##  [916]  0.722958721  0.964383048  0.690241655  0.816491310  0.506427243
    ##  [921]  0.810672158  0.673408615  0.713821444  0.837624255  0.595315209
    ##  [926]  0.502103911  0.884185949  0.537489552  0.653743402  0.728105628
    ##  [931]  0.213454974  0.470305090  0.891865018  0.344882415  0.841621555
    ##  [936]  0.458195737  0.526421929  0.857606109  0.664677684  0.703686570
    ##  [941]  0.314542648  0.736706949  0.434904196  0.895891553  0.847896537
    ##  [946]  0.876618178  0.153300443  0.180870914  0.746311837  0.826440839
    ##  [951]  0.631317740  0.886701670  0.664792226  0.408575343  0.927508501
    ##  [956]  0.732720984  0.805351135  0.486062110  0.252881838  0.824575752
    ##  [961]  0.653060772  0.258150087  0.356499293  0.657867070  0.923831178
    ##  [966]  0.370334325  0.702627914  0.814544030  0.721622577  0.519639736
    ##  [971]  0.832453630  0.850487227  0.726533933  0.555813362  0.919019518
    ##  [976]  0.845893213  0.647567961  0.343038308  0.876593289  0.648827930
    ##  [981]  0.884776998  0.695506714  0.584466809  0.789045210  0.871246442
    ##  [986]  0.474622389  0.558006853  0.575571394  0.261336550  0.717272721
    ##  [991]  0.713917503  0.551723217  0.701376470  0.474560758  0.937599249
    ##  [996]  0.649619769  0.702647653  0.601729923  0.780552548  0.768637576

The simulated values of the standard error are significantly higher than
the initial calculated value.

**4. W\&S Chapter 17**

``` r
# read in nutrient data
nutrient_data <- read.csv("./raw_data/all_data/chapter17/chap17q19GrasslandNutrientsPlantSpecies.csv")

# view nutrient data
nutrient_data
```

    ##    nutrients species
    ## 1          0      36
    ## 2          0      36
    ## 3          0      32
    ## 4          1      34
    ## 5          2      33
    ## 6          3      30
    ## 7          1      20
    ## 8          3      23
    ## 9          4      21
    ## 10         4      16

**a.** Draw a scatter plot of these data. Which variable should be the
explanatory variable (X), and which should be the response variable (Y)?

``` r
# Basic scatter plot using ggplot2
ggplot(nutrient_data, aes(x= species, y= nutrients)) +
  geom_point()
```

![](05_OKINEDO_UZEZI_2020_files/figure-gfm/scatter%20plot%20of%20nutrient%20data-1.png)<!-- -->
The explanatory variable(X) is “species” while the response variable(Y)
is “nutrients”.

**b.** What is the rate of change in the number of plant species
supported per nutrient type added? Provide a standard error for your
estimate.

``` r
fit.lm <-lm(nutrients ~ species, data = nutrient_data)
slope <- coef(fit.lm)
slope
```

    ## (Intercept)     species 
    ##   6.3106539  -0.1605215

``` r
# rate=(slope)nutrients+(intercept)
fit.lm$coefficient[1] + fit.lm$coefficient[2]
```

    ## (Intercept) 
    ##    6.150132

The rate of change is 6.150132 .

**c.** Add the least-squares regression line to your scatter plot. What
fraction of the variation in the number of plant species is “explained”
by the number of nutrients added?

``` r
# Fit a model
nutrient_lm <- lm(nutrients ~ species, data = nutrient_data)
# use anova to check p-value
anova_tb <- anova(nutrient_lm) %>%
  # use tidy() to summarize model statistical findings
  tidy()

anova_tb
```

    ## # A tibble: 2 x 6
    ##   term         df sumsq meansq statistic p.value
    ##   <chr>     <int> <dbl>  <dbl>     <dbl>   <dbl>
    ## 1 species       1  12.6  12.6       9.24  0.0161
    ## 2 Residuals     8  11.0   1.37     NA    NA

``` r
# R^2 = SS(regression)/SS(total)

anova_tb[1,3]/(anova_tb[1,3] + anova_tb[2, 3])
```

    ##       sumsq
    ## 1 0.5359785

The fraction of plant species explained by the nutrient added is
0.5359785

**d.** Test the null hypothesis of no treatment effect on the number of
plant species.

``` r
# test the null hypothesis by checking whether regression mean square is greater than residual mean square 

anova_tb[1,3] > anova_tb[2, 3]
```

    ##      sumsq
    ## [1,]  TRUE

The null hypothesis is false and hence, rejected because the regression
mean square value is higher than the residual mean square value as shown
in the ANOVA table in 4C above. That is; anova\_tb\[1,3\] \>
anova\_tb\[2, 3\].

**5. W\&S Chapter 17-25**

``` r
# load beetle data
beetle <- read.csv("./raw_data/all_data/chapter17/chap17q25BeetleWingsAndHorns.csv")

# view beetle data
beetle
```

    ##    hornSize wingMass
    ## 1     0.074    -42.8
    ## 2     0.079    -21.7
    ## 3     0.019    -18.8
    ## 4     0.017    -16.0
    ## 5     0.085    -12.8
    ## 6     0.081     11.6
    ## 7     0.011      7.6
    ## 8     0.023      1.6
    ## 9     0.005      3.7
    ## 10    0.007      1.1
    ## 11    0.004     -0.8
    ## 12   -0.002     -2.9
    ## 13   -0.065     12.1
    ## 14   -0.065     20.1
    ## 15   -0.014     21.2
    ## 16   -0.014     22.2
    ## 17   -0.132     20.1
    ## 18   -0.143     12.5
    ## 19   -0.177      7.0

**a.** Use these results to calculate the residuals

``` r
beetle_sims <- beetle %>%
  mutate('Predicted_relative_wing_mass_mg' = c(-9.9, -10.6, -2.6, 2.4, -11.4, -10.9, -1.6, -3.2, -0.8, -1.1, -0.7, 0.1, 8.5, 8.5, 1.7, 1.7, 17.4, 18.8, 23.3))

beetle_sims
```

    ##    hornSize wingMass Predicted_relative_wing_mass_mg
    ## 1     0.074    -42.8                            -9.9
    ## 2     0.079    -21.7                           -10.6
    ## 3     0.019    -18.8                            -2.6
    ## 4     0.017    -16.0                             2.4
    ## 5     0.085    -12.8                           -11.4
    ## 6     0.081     11.6                           -10.9
    ## 7     0.011      7.6                            -1.6
    ## 8     0.023      1.6                            -3.2
    ## 9     0.005      3.7                            -0.8
    ## 10    0.007      1.1                            -1.1
    ## 11    0.004     -0.8                            -0.7
    ## 12   -0.002     -2.9                             0.1
    ## 13   -0.065     12.1                             8.5
    ## 14   -0.065     20.1                             8.5
    ## 15   -0.014     21.2                             1.7
    ## 16   -0.014     22.2                             1.7
    ## 17   -0.132     20.1                            17.4
    ## 18   -0.143     12.5                            18.8
    ## 19   -0.177      7.0                            23.3

``` r
# Fit a model in the simulated beetle data.
beetle_lm <- lm(Predicted_relative_wing_mass_mg ~ hornSize, data = beetle_sims)

# calculate resduals from fit
residual_data <- residuals(beetle_lm)

# view residual values
residual_data
```

    ##           1           2           3           4           5           6 
    ## -0.31686109 -0.36069358 -0.23470372  4.50282927 -0.37329257 -0.39822658 
    ##           7           8           9          10          11          12 
    ## -0.28457174 -0.30976971 -0.27197275 -0.30950575 -0.30320626 -0.29060727 
    ##          13          14          15          16          17          18 
    ## -0.15831791 -0.15831791 -0.26540930 -0.26540930 -0.05096257 -0.09453109 
    ##          19 
    ## -0.05647017

**b.** Use your results from part (a) to produce a residual plot.

``` r
beet_new <- beetle %>%
  mutate('residuals' = residual_data)

residual_plot <- ggplot(beet_new, aes(x= hornSize, y= residuals)) +
  geom_point()

residual_plot
```

![](05_OKINEDO_UZEZI_2020_files/figure-gfm/produce%20a%20residual%20plot-1.png)<!-- -->

**c.** Use the graph provided and your residual plot to evaluate the
main assumptions of linear regression.

If the assumptions of normality and equal variance of residuals are not
met because of the following reasons;

1)  For the residual plot; the cloud of points above the horizontal line
    at zero are asymmetric with more points below the line than above
    the line. On the other hand, the graph provided also violates the
    assumptions of normality as there are more points above the
    horizontal zero line than below it.

2)  No noticeable curvature observed as we move from left to right along
    the x-axis in the residual plot.

3)  No equal variance of points above and below the line at all values
    of X in both the residual plot and the graph provided.

**d.** In light of your conclusions in part (c), what steps should be
taken?

1)  Log transformation to help meet the assumption of linear regression
2)  Square root transformation to solve the problem of unequal variance.

**e.** Do any other diagnostics misbehave?

Yes. Using noticeable curvature to evaluate the assumption of linear
regression is not an effective diagnostic method.

**6. W\&S Chapter 17-30**

``` r
# load nuclear data
nuclear <- read.csv("./raw_data/all_data/chapter17/chap17q30NuclearTeeth.csv")

# view data
nuclear
```

    ##    dateOfBirth deltaC14
    ## 1       1985.5       89
    ## 2       1983.5      109
    ## 3       1990.5       91
    ## 4       1987.5      127
    ## 5       1990.5       99
    ## 6       1984.5      110
    ## 7       1983.5      123
    ## 8       1989.5      105
    ## 9       1963.5      622
    ## 10      1971.7      262
    ## 11      1963.7      471
    ## 12      1990.5      112
    ## 13      1975.0      285
    ## 14      1970.2      439
    ## 15      1972.6      363
    ## 16      1971.8      391

**a.** What is the approximate slope of the regression line?

``` r
# fit a regression line through the data points
fit.lm <-lm( dateOfBirth ~ deltaC14, data = nuclear)
slope <- coef(fit.lm)
slope
```

    ##   (Intercept)      deltaC14 
    ## 1992.26736906   -0.05325906

The approximate slope of the regression line is **-16.7**

**b.** Which pair of lines shows the confidence bands? What do these
confidence bands tell us?

The confidence bands are the pair of lines closest to the regression
line. It shows the 95% confidence bands for the predicted mean date of
births at every amount of Carbon 14.

**c.** Which pair of lines shows the prediction interval? What does this
prediction interval tell us?

The prediction intervals are the pair of lines farther away from the
regression line. It shows the 95% prediction intervals for the predicted
date of births of each cadaver. n = 16.

**d.** Using predict() and geom\_ribbon() in ggplot2, reproduce the
above plot showing data, fit, fit interval, and prediction interval.

``` r
# fit interval
fit_nuclear <- predict(fit.lm,
                    nuclear,
                    interval = "confidence") %>%
                    
  as_tibble() %>%
  rename(lwr_ci = lwr,
         upr_ci = upr) 

nuclear_ci <- cbind(nuclear$deltaC14, fit_nuclear)

# prediction interval
predict_nuclear <- predict(fit.lm,
                           nuclear,
                           interval = "prediction") %>%
  as_tibble() %>%
  rename(lwr_pi = lwr,
         upr_pi = upr)

# create a new nuclear dataset which includes the prediction and fit values
new_nuclear <- nuclear_ci %>%
  # mutate prediction and fit values to make a new dataframe
   mutate('upper_pi' = predict_nuclear$upr_pi,
          'lower_pi' = predict_nuclear$lwr_pi,
          'date_of_birth' = nuclear$dateOfBirth)

# view new dataframe          
new_nuclear
```

    ##    nuclear$deltaC14      fit   lwr_ci   upr_ci upper_pi lower_pi date_of_birth
    ## 1                89 1987.527 1985.140 1989.914 1995.031 1980.024        1985.5
    ## 2               109 1986.462 1984.212 1988.712 1993.923 1979.002        1983.5
    ## 3                91 1987.421 1985.048 1989.794 1994.919 1979.922        1990.5
    ## 4               127 1985.503 1983.367 1987.640 1992.931 1978.076        1987.5
    ## 5                99 1986.995 1984.678 1989.312 1994.476 1979.514        1990.5
    ## 6               110 1986.409 1984.166 1988.652 1993.867 1978.950        1984.5
    ## 7               123 1985.717 1983.556 1987.877 1993.151 1978.282        1983.5
    ## 8               105 1986.675 1984.399 1988.951 1994.144 1979.207        1989.5
    ## 9               622 1959.140 1954.645 1963.635 1967.555 1950.726        1963.5
    ## 10              262 1978.313 1976.516 1980.111 1985.650 1970.976        1971.7
    ## 11              471 1967.182 1964.108 1970.256 1974.932 1959.433        1963.7
    ## 12              112 1986.302 1984.072 1988.532 1993.757 1978.848        1990.5
    ## 13              285 1977.089 1975.238 1978.939 1984.439 1969.739        1975.0
    ## 14              439 1968.887 1966.086 1971.688 1976.532 1961.242        1970.2
    ## 15              363 1972.934 1970.703 1975.166 1980.390 1965.479        1972.6
    ## 16              391 1971.443 1969.018 1973.868 1978.958 1963.928        1971.8

``` r
ggplot(data = new_nuclear,
       mapping = aes(x = nuclear$deltaC14,
                     y = date_of_birth)) +
  #prediction interval
  geom_ribbon(mapping = aes(ymin = lower_pi,
                            ymax = upper_pi),
              alpha = 0.5) +
  # fit interval - just coefficient error (precision)
  geom_ribbon(mapping = aes(ymin = lwr_ci,
                            ymax = upr_ci),
              color = "red",
              alpha = 0.5) +
  geom_point() +
  stat_smooth(method = "lm") #shows error around our FIT
```

    ## `geom_smooth()` using formula 'y ~ x'

![](05_OKINEDO_UZEZI_2020_files/figure-gfm/plot-1.png)<!-- -->
