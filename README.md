Introduction
------------

PupillometryR is a package to pre-process and then analyze simple pupil
experiments in R. The best way to download it is to open up R in
RStudio, and download it after [downloading
devtools](https://www.r-project.org/nosvn/pandoc/devtools.html). It
first needs to be downloaded before running, if you have devtools, it
can be done directly from the console in R:

    devtools::install_github("samhforbes/PupillometryR")

Once we have the package downloaded, we need to load in the package to
get started:

    library(PupillometryR)

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: ggplot2

This package is designed to make dealing with pupil data (perhaps more
traditionally done in MATLAB) easier to wrangle in R. It makes heavy use
of a few packages which should be acknowledged here, especially the
(excellent) packages fda and signal.

As well as the above packages, it is very important to note that the
type of analysis shown here has been available in MATLAB for a while,
and there is an excellent tutorial on it, which I thoroughly recommend
reading first, written by Sylvain Sirois,
[here:](https://oraprdnt.uqtr.uquebec.ca/pls/public/gscw031?owa_no_site=314&owa_no_fiche=3&owa_bottin=https://oraprdnt.uqtr.uquebec.ca/pls/public/gscw031?owa_no_site=314&owa_no_fiche=3&owa_bottin=).

It’s worth making sure that your setup and experiment do facilitate the
use of pupillometry - it may not be suited for all kinds of experiment.

Getting started
---------------

We will first run through an example analysis with the data provided in
the package, which, again, comes from Sylvain Sirois’ tutorial on [his
webpage](https://oraprdnt.uqtr.uquebec.ca/pls/public/gscw031?owa_no_site=314&owa_no_fiche=3&owa_bottin=https://oraprdnt.uqtr.uquebec.ca/pls/public/gscw031?owa_no_site=314&owa_no_fiche=3&owa_bottin=).

The first thing I would recommend doing is having a close look at the
pupil data. Eyetrackers have a couple of different ways of dealing with
this, so it’s important to know a few things:

1.  What unit of measurement is being used
2.  What value is given to missing data or blinks (typically ., -1, or
    *NA*)
3.  What framerate were you recording at, and is this consistent.

The data here is an eyetracking experiment with hard and easy trials,
performed on 9 participants. Participant 8 needs to be removed (more
details on Sylvain’s tutorial).

    data("pupil_data")

    #Check that IDs are not numeric
    pupil_data$ID <- as.character(pupil_data$ID)
    #remove participant number 8, who had problematic data
    pupil_data <- subset(pupil_data, ID != 8)
    #blinks were registered as -1, so replace with NAs
    pupil_data$LPupil[pupil_data$LPupil == -1] <- NA
    pupil_data$RPupil[pupil_data$RPupil == -1] <- NA

First up, we need to put the data into pupillometryR format for further
analysis

    Sdata <- make_pupillometryr_data(data = pupil_data,
                                     subject = ID,
                                     trial = Trial,
                                     time = Time,
                                     condition = Type)

In the current data, it is not a concern, but there may be a situation
where certain timebins are missing from your data. This can be fixed
here, and we will look at the raw data:

    new_data <- replace_missing_data(data = Sdata,
                                     rpupil = RPupil,
                                     lpupil = LPupil)

    ## Warning in replace_missing_data(data = Sdata, rpupil = RPupil, lpupil
    ## = LPupil): replace_missing_data will only help if you have missing
    ## timepoints, and a reliable time column.

    head(new_data)

    ##   ID Trial      Time   RPupil   LPupil Timebin Type
    ## 1  1 Easy1  16.66667 3.233615 2.935160       1 Easy
    ## 2  1 Easy1  33.33333 3.216972 2.919698       2 Easy
    ## 3  1 Easy1  50.00000 3.205405 2.924402       3 Easy
    ## 4  1 Easy1  66.66667 3.217505 2.935777       4 Easy
    ## 5  1 Easy1  83.33333 3.214905 2.928921       5 Easy
    ## 6  1 Easy1 100.00000 3.211777 2.922048       6 Easy

Equally, if your data is not cut to the time windows that you are
interested in, the subset\_data function allows trimming. PupillometryR
has some built-in plotting functions to allow you to look at certain
data types. You simply need to specify a pupil column to display, and
how you would like the data displayed in groups. The plots are ggplot
items, so can be edited with themes, and arguments such as ylab(). Below
we display it first by condition, then by subject:

    plot(new_data, pupil = LPupil, group = 'condition')

    ## Warning: Removed 3639 rows containing non-finite values (stat_summary).

![](README_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    plot(new_data, pupil = LPupil, group = 'subject') 

    ## Warning: Removed 3639 rows containing non-finite values (stat_summary).

![](README_files/figure-markdown_strict/unnamed-chunk-6-2.png)

Smoothing
---------

PupillometryR offers a few smoothing options to make processing the data
a little easier. We’ll do the full set here. A great reference for these
is Sylvain’s tutorial, and also Jackson and Sirois, 2009, Dev. Sci.
First off, we can regress one pupil against the other to get some
measure of smoothing.

    regressed_data <- regress_data(data = new_data,
                                   pupil1 = RPupil,
                                   pupil2 = LPupil)

Now that we have done that, we want the mean of the two pupil sizes, so
let’s see how that looks:

    mean_data <- calculate_mean_pupil_size(data = regressed_data, rpupil = RPupil, lpupil = LPupil)

    plot(mean_data, pupil = mpupil, group = 'subject')

    ## Warning: Removed 3710 rows containing non-finite values (stat_summary).

![](README_files/figure-markdown_strict/unnamed-chunk-8-1.png)

PupillometryR offers 3 filter types: A hanning window, a low-pass
butterworth filter, and a median filter. The low-pass filter can be a
little unstable at the beginning and end of each trial, so it’s worth
looking at your data to see if it’s appropriate. Here we will use the
hanning filter. The degree gives the size of the rolling window.

    filtered_data <- filter_data(data = mean_data,
                                 pupil = mpupil,
                                 filter = 'hanning',
                                 degree = 11)



    plot(filtered_data, pupil = mpupil, group = 'subject')

    ## Warning: Removed 4141 rows containing non-finite values (stat_summary).

![](README_files/figure-markdown_strict/unnamed-chunk-9-1.png)

The next step is to interpolate across blinks. Filtering before the
interpolation generally allows more sensible interpolation, but the way
this is done varies a bit on the data, and you will see examples without
this. We can interpolate in this package either linear or cubic, but
again, it’s best to always check your data afterwards to make sure it
looks the way you might expect. Here we opt for the linear
interpolation:

    int_data <- interpolate_data(data = filtered_data,
                                 pupil = mpupil,
                                 type = 'linear')

    #plot
    plot(int_data, pupil = mpupil, group = 'subject')

![](README_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Baselining the data is a powerful way of making sure we control for
between-participant variance of average pupil size. If we are looking at
analyses that are largely within-subject, as we do here, this may not be
such an issue, but we will do this anyway. This function allows us to
baseline to the mean pupil size within a time window. Here we are just
taking the first 100ms of the trial. If your baseline period is just
outside of your analysis window (which it often will be), you can use
subset\_data() to remove that after baselining.

    base_data <- baseline_data(data = int_data,
                               pupil = mpupil,
                               start = 0,
                               stop = 100)

    plot(base_data, pupil = mpupil, group = 'subject')

![](README_files/figure-markdown_strict/unnamed-chunk-11-1.png)

Window analyses
---------------

PupillometryR gives us a couple of options for window analysis. One is
overall averages, the other is to break the data up into discrete time
windows, and to analyse them. First we will opt for the overall
averages. We can plot these with any of boxplots, violin plots, or,
since the new edition, Micah Allen-esque raincloud plots (Allen et al.,
2018).

    window <- create_window_data(data = base_data,
                                 pupil = mpupil)

    plot(data = window, pupil = mpupil, windows = F, geom = 'boxplot')

![](README_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    head(window)

    ##   ID Type       mpupil
    ## 1  1 Easy -0.228407146
    ## 2  1 Hard -0.078613427
    ## 3  2 Easy -0.094864658
    ## 4  2 Hard  0.008134712
    ## 5  3 Easy -0.095396599
    ## 6  3 Hard  0.009062905

We could then simply analyse this with a t-test if we wished.

    t.test(mpupil ~ Type, paired = T, data = window)

    ## 
    ##  Paired t-test
    ## 
    ## data:  mpupil by Type
    ## t = -3.7021, df = 7, p-value = 0.007634
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.20765180 -0.04577778
    ## sample estimates:
    ## mean of the differences 
    ##              -0.1267148

Alternatively, we may wish to look at the data in chunks. Here we group
the data in to 2000ms timebins for analysis (and we will opt for the
raincloud plot in this instance:

    timeslots <- create_time_windows(data = base_data,
                                     pupil = mpupil,
                                     breaks = c(0, 2000, 4000, 6000, 8000, 10000))

    plot(timeslots, pupil = mpupil, windows = T, geom = 'raincloud')

![](README_files/figure-markdown_strict/unnamed-chunk-14-1.png)

    head(timeslots)

    ##   ID Type Window      mpupil
    ## 1  1 Easy      1 -0.01416402
    ## 2  1 Easy      2 -0.19557003
    ## 3  1 Easy      3 -0.38812531
    ## 4  1 Easy      4 -0.29171423
    ## 5  1 Easy      5 -0.24969595
    ## 6  1 Hard      1  0.02258996

And again, we could analyse this with an anova

    car::Anova(lm(mpupil ~ Window * Type, data = timeslots))

    ## Anova Table (Type II tests)
    ## 
    ## Response: mpupil
    ##              Sum Sq Df F value    Pr(>F)    
    ## Window      1.04099  4 12.5896 9.038e-08 ***
    ## Type        0.32083  1 15.5205 0.0001907 ***
    ## Window:Type 0.09552  4  1.1552 0.3380710    
    ## Residuals   1.44701 70                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Modelling with Generalised Additive Models
------------------------------------------

Here we interfact with the mgcv package, an exceptionally powerful
package for GAM data, by Simon Wood. I strongly encourage reading the
vignette and checking out some of the great online tutorials (of which
there are plenty; I quite like Michael Clark’s one
[here](https://m-clark.github.io/generalized-additive-models/case_for_gam.html))
before proceeding with these.

We have to do a little bit of setting up of our variables (scaling and
centering) before we continue. I need to make some variables numeric
(the ones with an n on the end), and I am using the way trials are
labelled to make this a numeric variable (this would probably be
different for your data).

    library(mgcv)

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-24. For overview type 'help("mgcv-package")'.

    base_data$IDn <- as.numeric(base_data$ID)
    base_data$Typen <- ifelse(base_data$Type == 'Easy', .5, -.5)
    base_data$Trialn <- as.numeric(substr(base_data$Trial, 5, 5))
    base_data$Trialn <- ifelse(base_data$Typen == .5, base_data$Trialn, base_data$Trialn + 3)
    base_data$ID <- as.factor(base_data$ID)

    model_data <- base_data

Right, let’s proceed with setting up a simple model. It’s recommended
for the amount of data points we might have for PupillometryR, bams
might be a better option, but both gam() and bam() will work.

    m1 <- bam(mpupil ~ s(Time) + s(Time,  by = Typen),
              data = base_data,
              family = gaussian)

    summary(m1)

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## mpupil ~ s(Time) + s(Time, by = Typen)
    ## 
    ## Parametric coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.119482   0.001183    -101   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                 edf Ref.df      F p-value    
    ## s(Time)       8.581  8.950 1112.2  <2e-16 ***
    ## s(Time):Typen 8.335  9.328  421.8  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.325   Deviance explained = 32.6%
    ## fREML =  -5341  Scale est. = 0.040273  n = 28800

We can use our default plotting function to see how it looks compared to
the raw data, just by specifying the model= argument.

    plot(model_data, pupil = mpupil, group = 'condition', model = m1)

![](README_files/figure-markdown_strict/unnamed-chunk-18-1.png)

Of course there is the fact that we expect there to by some variation by
trial, and that we should expect there to be differences for each
participant. So a much better specified model would be better, as we
have to allow the effect of time to vary by participant. I am no expert
on GAMs by any stretch of the imagination, but a better model might look
like the below

    m2 <- bam(mpupil ~ s(Time) + s(Time,  by = Typen) +
                s(Time, ID, bs = 'fs', m = 1) + te(Time, Trialn),
              data = base_data,
              family = gaussian)

    plot(model_data, pupil = mpupil, group = 'condition', model = m2)

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.png)

    summary(m2)

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## mpupil ~ s(Time) + s(Time, by = Typen) + s(Time, ID, bs = "fs", 
    ##     m = 1) + te(Time, Trialn)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.12900    0.03194  -4.039 5.38e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                    edf Ref.df       F p-value    
    ## s(Time)          5.636  5.941   0.647   0.691    
    ## s(Time):Typen    9.327  9.888  66.337  <2e-16 ***
    ## s(Time,ID)      62.925 71.000 231.474  <2e-16 ***
    ## te(Time,Trialn) 20.384 21.023  34.765  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.578   Deviance explained = 57.9%
    ## fREML = -11908  Scale est. = 0.025214  n = 28800

The summary from our second model indicates that there is evidence for a
bunch of effects - crucially we still see this effect of condition. But
how and when do they diverge???

Estimating divergences with functional data analysis
----------------------------------------------------

The above analyses may well suffice for what we have planned. However,
sometimes it’s useful for analysis to examine change over time,
especially how and when two conditions diverge, and we can do that with
Functional Data Analysis (FDA). This part of the package makes usage of
the fda package. The complete guide really has been written in 1997 by
Ramsay and Silverman, and there is a very helpful website on FDA
[here](https://www.psych.mcgill.ca/misc/fda/resources.html). This
package is currently only setup to use this analysis for two-condition
experiments, but I hope to add options for functional ANOVA in the
future.

To do this, first we want get the difference between the two conditions
for each participant. By default this package wil take condition 2 -
condition 1, so reorder the factors if required.

    differences <- create_difference_data(data = base_data,
                                          pupil = mpupil)

    ## Hard minus Easy  -- relevel condition if this is not the intended outcome

    plot(differences, pupil = mpupil, geom = 'line')

![](README_files/figure-markdown_strict/unnamed-chunk-20-1.png)

We can now convert this to a functional data structure, made up of
curves. To do this for this data we are going to make it up of cubics
(order = 4) with 10 knots (basis = 10). The appropriate numbers here
will depend on your dataset, and I strongly advise consulting Ramsay and
Silverman’s book, and the FDA website, as well as Sylvain’s paper
mentioned above. This interfaces with the fda package.

    spline_data <- create_functional_data(data = differences,
                                          pupil = mpupil,
                                          basis = 10,
                                          order = 4)


    plot(spline_data, pupil = mpupil, geom = 'line', colour = 'blue')

![](README_files/figure-markdown_strict/unnamed-chunk-21-1.png)

That looks like it’s done a pretty good job capturing the data. The
advantage of this kind of analysis is that we can treat each curve as a
function, and run a single functional t-test to work out during which
window there are divergences. This package allows us to do that
directly, and to observe the results.

    ft_data <- run_functional_t_test(data = spline_data,
                                     pupil = mpupil,
                                     alpha = 0.05)

    ## critical value for n = 8 is 2.364624

    plot(ft_data, show_divergence = T, colour = 'red', fill = 'orange')

![](README_files/figure-markdown_strict/unnamed-chunk-22-1.png)

If show\_divergence is set to TRUE, the plot will highlight where the
two conditions diverge at the alpha you set.

Acknowledgements
----------------

This package has had suggestions, encouragement, and help from a number
of people, but I wish to especially highlight Sylvain Sirois and Mihaela
Duta, whose input has been instrumental.

References
----------

\[1\] Jackson, I., & Sirois, S. (2009). Infant cognition: Going full
factorial with pupil dilation. *Developmental Science*, 12(4), 670-679.
<http://doi.org/10.1111/j.1467-7687.2008.00805.x>

\[2\] Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., & Kievit,
R. (2018). Raincloud plots: a multi-platform tool for robust data
visualization. *PeerJ Preprints*, 6.
<http://doi.org/10.1044/1092-4388(2010/09-0205)>

\[3\] Ramsay, J.O., & Silverman, B.W. (1997). *Functional data
analysis*. New York: Springer-Verlag.
