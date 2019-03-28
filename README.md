Introduction
------------

PupillometryR is a package to pre-process and then analyze simple pupil
experiments in R. The best way to download it is to open up R in
RStudio, and download it after [downloading
devtools](https://www.r-project.org/nosvn/pandoc/devtools.html). It
first needs to be downloaded before running, if you have devtools, it
can be done directly from the console in R:

    devtools::install_github('samhforbes/PupillometryR')

Once we have the package downloaded, we need to load in the package to
get started:

    library(PupillometryR)

    ## Loading required package: dplyr

    ## Warning: package 'dplyr' was built under R version 3.5.3

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

The plotting is also a theme of this tutorial, so I will set a nice
theme that makes the plots look pretty:

    library(ggplot2)
    theme_set(theme_classic(base_size = 12))

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

    new_data <- replace_missing_data(data = Sdata)

    ## Warning in replace_missing_data(data = Sdata): replace_missing_data will
    ## only help if you have missing timepoints, and a reliable time column.

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

![](README_files/figure-markdown_strict/unnamed-chunk-7-1.svg)

    plot(new_data, pupil = LPupil, group = 'subject') 

    ## Warning: Removed 3639 rows containing non-finite values (stat_summary).

![](README_files/figure-markdown_strict/unnamed-chunk-7-2.svg)

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

    mean_data <- calculate_mean_pupil_size(data = regressed_data, 
                                           pupil1 = RPupil, 
                                           pupil2 = LPupil)

    plot(mean_data, pupil = mean_pupil, group = 'subject')

    ## Warning: Removed 3710 rows containing non-finite values (stat_summary).

![](README_files/figure-markdown_strict/unnamed-chunk-9-1.svg)

PupillometryR offers 3 filter types: A hanning window, a low-pass
butterworth filter, and a median filter. The low-pass filter can be a
little unstable at the beginning and end of each trial, so it’s worth
looking at your data to see if it’s appropriate. Here we will use the
hanning filter. The degree gives the size of the rolling window.

    filtered_data <- filter_data(data = mean_data,
                                 pupil = mean_pupil,
                                 filter = 'hanning',
                                 degree = 11)

    ## Performing hanning filter

    plot(filtered_data, pupil = mean_pupil, group = 'subject')

    ## Warning: Removed 4141 rows containing non-finite values (stat_summary).

![](README_files/figure-markdown_strict/unnamed-chunk-10-1.svg)

The next step is to interpolate across blinks. Filtering before the
interpolation generally allows more sensible interpolation, but the way
this is done varies a bit on the data, and you will see examples without
this. We can interpolate in this package either linear or cubic, but
again, it’s best to always check your data afterwards to make sure it
looks the way you might expect. Here we opt for the linear
interpolation:

    int_data <- interpolate_data(data = filtered_data,
                                 pupil = mean_pupil,
                                 type = 'linear')

    ## Performing linear interpolation

    #plot
    plot(int_data, pupil = mean_pupil, group = 'subject')

![](README_files/figure-markdown_strict/unnamed-chunk-11-1.svg)

Baselining the data is a powerful way of making sure we control for
between-participant variance of average pupil size. If we are looking at
analyses that are largely within-subject, as we do here, this may not be
such an issue, but we will do this anyway. This function allows us to
baseline to the mean pupil size within a time window. Here we are just
taking the first 100ms of the trial. If your baseline period is just
outside of your analysis window (which it often will be), you can use
subset\_data() to remove that after baselining.

    base_data <- baseline_data(data = int_data,
                               pupil = mean_pupil,
                               start = 0,
                               stop = 100)

    plot(base_data, pupil = mean_pupil, group = 'subject')

![](README_files/figure-markdown_strict/unnamed-chunk-12-1.svg)

Window analyses
---------------

PupillometryR gives us a couple of options for window analysis. One is
overall averages, the other is to break the data up into discrete time
windows, and to analyse them. First we will opt for the overall
averages. We can plot these with any of boxplots, violin plots, or,
since the new edition, Micah Allen-esque raincloud plots (Allen et al.,
2018).

    window <- create_window_data(data = base_data,
                                 pupil = mean_pupil)

    plot(data = window, pupil = mean_pupil, windows = F, geom = 'boxplot')

![](README_files/figure-markdown_strict/unnamed-chunk-13-1.svg)

    head(window)

    ##   ID Type   mean_pupil
    ## 1  1 Easy -0.305353938
    ## 2  1 Hard -0.078613427
    ## 3  2 Easy -0.094864658
    ## 4  2 Hard  0.008134712
    ## 5  3 Easy -0.166532829
    ## 6  3 Hard  0.009062905

We could then simply analyse this with a t-test if we wished.

    t.test(mean_pupil ~ Type, paired = T, data = window)

    ## 
    ##  Paired t-test
    ## 
    ## data:  mean_pupil by Type
    ## t = -3.6322, df = 7, p-value = 0.008374
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.25319000 -0.05351629
    ## sample estimates:
    ## mean of the differences 
    ##              -0.1533531

Alternatively, we may wish to look at the data in chunks. Here we group
the data in to 2000ms timebins for analysis (and we will opt for the
raincloud plot in this instance:

    timeslots <- create_time_windows(data = base_data,
                                     pupil = mean_pupil,
                                     breaks = c(0, 2000, 4000, 6000, 8000, 10000))

    plot(timeslots, pupil = mean_pupil, windows = T, geom = 'raincloud')

![](README_files/figure-markdown_strict/unnamed-chunk-15-1.svg)

    head(timeslots)

    ##   ID Type Window  mean_pupil
    ## 1  1 Easy      1 -0.08540123
    ## 2  1 Easy      2 -0.27392937
    ## 3  1 Easy      3 -0.46648466
    ## 4  1 Easy      4 -0.37007358
    ## 5  1 Easy      5 -0.32805530
    ## 6  1 Hard      1  0.02258996

And again, we could analyse this with an anova

    car::Anova(lm(mean_pupil ~ Window * Type, data = timeslots))

    ## Anova Table (Type II tests)
    ## 
    ## Response: mean_pupil
    ##              Sum Sq Df F value    Pr(>F)    
    ## Window      1.04909  4 13.1048 5.099e-08 ***
    ## Type        0.46997  1 23.4826 7.315e-06 ***
    ## Window:Type 0.09738  4  1.2164    0.3117    
    ## Residuals   1.40094 70                      
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

    m1 <- bam(mean_pupil ~ s(Time) + s(Time,  by = Typen),
              data = base_data,
              family = gaussian)

    summary(m1)

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## mean_pupil ~ s(Time) + s(Time, by = Typen)
    ## 
    ## Parametric coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.124807   0.001222  -102.1   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                 edf Ref.df      F p-value    
    ## s(Time)       8.576  8.949 1050.5  <2e-16 ***
    ## s(Time):Typen 8.283  9.291  533.8  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.333   Deviance explained = 33.3%
    ## fREML = -4400.2  Scale est. = 0.042993  n = 28800

We can use our default plotting function to see how it looks compared to
the raw data, just by specifying the model= argument.

    plot(model_data, pupil = mean_pupil, group = 'condition', model = m1)

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.svg)

Of course there is the fact that we expect there to by some variation by
trial, and that we should expect there to be differences for each
participant. So a much better specified model would be better, as we
have to allow the effect of time to vary by participant. I am no expert
on GAMs by any stretch of the imagination, but a better model might look
like the below

    m2 <- bam(mean_pupil ~ s(Time) + s(Time,  by = Typen) +
                s(Time, ID, bs = 'fs', m = 1) + te(Time, Trialn),
              data = base_data,
              family = gaussian)

    plot(model_data, pupil = mean_pupil, group = 'condition', model = m2)

![](README_files/figure-markdown_strict/unnamed-chunk-20-1.svg)

    summary(m2)

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## mean_pupil ~ s(Time) + s(Time, by = Typen) + s(Time, ID, bs = "fs", 
    ##     m = 1) + te(Time, Trialn)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.13229    0.03183  -4.156 3.25e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                    edf Ref.df       F p-value    
    ## s(Time)          5.739  6.057   1.161   0.325    
    ## s(Time):Typen    9.231  9.854  86.692  <2e-16 ***
    ## s(Time,ID)      62.364 71.000 174.773  <2e-16 ***
    ## te(Time,Trialn) 20.170 20.917  30.410  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.541   Deviance explained = 54.3%
    ## fREML = -9617.3  Scale est. = 0.029578  n = 28800

The summary from our second model indicates that there is evidence for a
bunch of effects - crucially we still see this effect of condition. But
how and when do they diverge???

(In fact, TJ Mahr came up with an elegant solution for this using GAM
methods with the itsadug package, which I will [link
to](https://gist.github.com/tjmahr/0d2b41ea1525205a99b19770fc916a90)
rather than take credit for)

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
                                          pupil = mean_pupil)

    ## Hard minus Easy  -- relevel condition if this is not the intended outcome

    plot(differences, pupil = mean_pupil, geom = 'line')

![](README_files/figure-markdown_strict/unnamed-chunk-21-1.svg)

We can now convert this to a functional data structure, made up of
curves. To do this for this data we are going to make it up of cubics
(order = 4) with 10 knots (basis = 10). The appropriate numbers here
will depend on your dataset, and I strongly advise consulting Ramsay and
Silverman’s book, and the FDA website, as well as Sylvain’s paper
mentioned above. This interfaces with the fda package.

    spline_data <- create_functional_data(data = differences,
                                          pupil = mean_pupil,
                                          basis = 10,
                                          order = 4)


    plot(spline_data, pupil = mean_pupil, geom = 'line', colour = 'blue')

![](README_files/figure-markdown_strict/unnamed-chunk-22-1.svg)

That looks like it’s done a pretty good job capturing the data. The
advantage of this kind of analysis is that we can treat each curve as a
function, and run a single functional t-test to work out during which
window there are divergences. This package allows us to do that
directly, and to observe the results.

    ft_data <- run_functional_t_test(data = spline_data,
                                     pupil = mean_pupil,
                                     alpha = 0.05)

    ## critical value for n = 8 is 2.364624

    plot(ft_data, show_divergence = T, colour = 'red', fill = 'orange')

![](README_files/figure-markdown_strict/unnamed-chunk-23-1.svg)

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
