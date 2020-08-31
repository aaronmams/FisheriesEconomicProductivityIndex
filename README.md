Measuring Output for U.S. Commercial Fisheries From Theory to Practice
======================================================================

Emily Markowitz<sup>1</sup>
(<a href="mailto:Emily.Markowitz@noaa.gov" class="email">Emily.Markowitz@noaa.gov</a>)

Sun Ling Wang<sup>2</sup>
(<a href="mailto:Sun-Ling.Wang@noaa.gov" class="email">Sun-Ling.Wang@noaa.gov</a>)

<sup>1</sup>Contractor, ECS Federal in support of NOAA Fisheries Office
of Science and Technology Economics & Social Analysis Division

<sup>2</sup>On detail with the NOAA Fisheries Office of Science and
Technology Economics & Social Analysis Division

> \*The views expressed are those of the author and should not be
> attributed to the NOAA, ECS or ERS

A quick message from our sponcers: NOAA README
==============================================

> This repository is a scientific product and is not official
> communication of the National Oceanic and Atmospheric Administration,
> or the United States Department of Commerce. All NOAA GitHub project
> code is provided on an ‘as is’ basis and the user assumes
> responsibility for its use. Any claims against the Department of
> Commerce or Department of Commerce bureaus stemming from the use of
> this GitHub project will be governed by all applicable Federal law.
> Any reference to specific commercial products, processes, or services
> by service mark, trademark, manufacturer, or otherwise, does not
> constitute or imply their endorsement, recommendation or favoring by
> the Department of Commerce. The Department of Commerce seal and logo,
> or the seal and logo of a DOC bureau, shall not be used in any manner
> to imply endorsement of any commercial product or activity by DOC or
> the United States Government.

Study Purpose
=============

-   Develop alternative approaches to measure national and regional
    fishery outputs for productivity measurements.

-   Evaluate the impacts of missing data and other issues on output
    estimates.

Theoretical Framework: Törnqvist index
======================================

### A Flexible Function and Superlative Quantity Index (Diewert 1976)

\#Math Theory: General Total Factor Productivity (*T**F**P*) Equation

The general form of the *T**F**P* can be measured as aggregate output
(*Y*) divided by real total inputs (*X*). Rates of TFP growth are
constructed using the Törnqvist index approach. The TFP growth over two
time periods is defined as:

$$ln(TFP\_t/TFP\_{t-1}) = \\sum\_{i=1}^n((\\frac{R\_{t,i} + R\_{t-1,i}}{2}) \* ln(\\frac{Y\_{t,i}}{Y\_{t-1,i}}))) - \\sum\_{j=1}^m((\\frac{W\_{j,t} + W\_{j,t-1}}{2}) \* ln(\\frac{X\_{j,t}}{X\_{j,t-1}})))$$

Such that:

-   Output represents
    $\\sum\_{i=1}^n((\\frac{R\_{it} + R\_{it-1}}{2}) \* ln(\\frac{Y\_{it}}{Y\_{it-1}}))$

-   Input represents
    $\\sum\_{j=1}^n((\\frac{W\_{jt} + W\_{jt-1}}{2}) \* ln(\\frac{X\_{jt}}{X\_{jt-1}}))$

where:

-   *Y*<sub>*i*</sub> = individual outputs. This will later be refered
    to as *Q*<sub>*i*</sub> in the following equations.

-   *X*<sub>*j*</sub> = individual inputs

-   *R*<sub>*i*</sub> = output revenue shares

-   *W*<sub>*j*</sub> = input cost shares

-   *t* and *t* − 1 = time, where 1 is the minimum year in the dataset

-   *i* = fishery category, e.g., Finfish (=1), Shellfish (=2)

-   *s* = species, e.g., Salmon, Alewife, Surf Clams

------------------------------------------------------------------------

\#Output Method: From Quantity to Quantity Measures

\#\#\#Variable Summary

Variables

-   *Q* = individual quantity outputs in pounds (lbs).

-   *V* = individual value outputs in dollars ($)

-   *Q**E* and *V**E* = simple sum of Quantity (Q) and Value (V)

-   *R* = output revenue shares

-   *b**a**s**e**y**r* is the year to base all indicides from

Subscript Inidicies

-   *t* and *t* − 1 are time subscripts, where 1 is the minimum year in
    the dataset

-   *i* is category, e.g., Finfish (=1), Shellfish (=2)

-   *s* is species, e.g., Salmon, Alewife, Surf Clams

\#\#\#Data requirements and source

The Tornqvist quantity index requires data on quantity and revenue
shares. We employ landings quantity (pounds) and landings value ($USD)
data by year, state, and species.

-   Data source: [Fisheries One Stop Shop downloaded August 13
    2020](https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200::::::)

-   More information about the data: [Commericial Fisheries Landings
    Data](https://www.fisheries.noaa.gov/national/sustainable-fisheries/commercial-fisheries-landings)

Documentation
=============

For specifics about how the Quantitative and Price Methods are derived,
please read this
[Documentation](https://github.com/emilyhmarkowitz/FisheriesEconomicProductivityIndex/blob/master/rscripts/ProductivityIndex_DocumentationSummary.docx).

File Organization
=================

All of the functions are stored in the
[*ProductivityIndex\_Functions.R*](https://github.com/emilyhmarkowitz/FisheriesEconomicProductivityIndex/blob/master/rscripts/ProductivityIndex_Functions.R)
r script.

Main fuctions of interest are:

-   PriceMethodOutput

-   PriceMethodOutput.Category

-   QuantityMethodOutput

-   QuantityMethodOutput.Category

R Package *FisheriesEconomicProductivityIndex* forthcomming.
