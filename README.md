Measuring Output for U.S. Commercial Fisheries — From Theory to Practice
========================================================================

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

Here is the origional data:

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">year</th>
<th style="text-align: right;">V1_1Salmon</th>
<th style="text-align: right;">Q1_1Salmon</th>
<th style="text-align: right;">V1_2Cod</th>
<th style="text-align: right;">Q1_2Cod</th>
<th style="text-align: right;">V2_1Shrimp</th>
<th style="text-align: right;">Q2_1Shrimp</th>
<th style="text-align: right;">V2_2Clam</th>
<th style="text-align: right;">Q2_2Clam</th>
<th style="text-align: right;">V1_3Flounder</th>
<th style="text-align: left;">Q1_3Flounder</th>
<th style="text-align: right;">V1_4SeaBass</th>
<th style="text-align: right;">Q1_4SeaBass</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1</td>
<td style="text-align: right;">2007</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">2800</td>
<td style="text-align: right;">2000</td>
<td style="text-align: right;">800</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">1000</td>
<td style="text-align: right;">150</td>
<td style="text-align: right;">1000</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">1000</td>
</tr>
<tr class="even">
<td>2</td>
<td style="text-align: right;">2008</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">2700</td>
<td style="text-align: right;">1900</td>
<td style="text-align: right;">1000</td>
<td style="text-align: right;">120</td>
<td style="text-align: right;">1200</td>
<td style="text-align: right;">160</td>
<td style="text-align: right;">1200</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">120</td>
<td style="text-align: right;">1200</td>
</tr>
<tr class="odd">
<td>3</td>
<td style="text-align: right;">2009</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">2900</td>
<td style="text-align: right;">2000</td>
<td style="text-align: right;">900</td>
<td style="text-align: right;">110</td>
<td style="text-align: right;">900</td>
<td style="text-align: right;">140</td>
<td style="text-align: right;">900</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">110</td>
<td style="text-align: right;">900</td>
</tr>
<tr class="even">
<td>4</td>
<td style="text-align: right;">2010</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">3000</td>
<td style="text-align: right;">2500</td>
<td style="text-align: right;">700</td>
<td style="text-align: right;">90</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">90</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td>5</td>
<td style="text-align: right;">2011</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">3100</td>
<td style="text-align: right;">2400</td>
<td style="text-align: right;">900</td>
<td style="text-align: right;">80</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">80</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td>6</td>
<td style="text-align: right;">2012</td>
<td style="text-align: right;">150</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">2900</td>
<td style="text-align: right;">2300</td>
<td style="text-align: right;">1000</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td>7</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">180</td>
<td style="text-align: right;">11</td>
<td style="text-align: right;">2800</td>
<td style="text-align: right;">2000</td>
<td style="text-align: right;">1200</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">1000</td>
<td style="text-align: right;">140</td>
<td style="text-align: right;">1000</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">1000</td>
</tr>
<tr class="even">
<td>8</td>
<td style="text-align: right;">2014</td>
<td style="text-align: right;">170</td>
<td style="text-align: right;">11</td>
<td style="text-align: right;">3200</td>
<td style="text-align: right;">2300</td>
<td style="text-align: right;">1100</td>
<td style="text-align: right;">110</td>
<td style="text-align: right;">900</td>
<td style="text-align: right;">110</td>
<td style="text-align: right;">900</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">900</td>
</tr>
<tr class="odd">
<td>9</td>
<td style="text-align: right;">2015</td>
<td style="text-align: right;">200</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">3500</td>
<td style="text-align: right;">2400</td>
<td style="text-align: right;">1000</td>
<td style="text-align: right;">90</td>
<td style="text-align: right;">1000</td>
<td style="text-align: right;">130</td>
<td style="text-align: right;">1000</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">1000</td>
</tr>
<tr class="even">
<td>10</td>
<td style="text-align: right;">2016</td>
<td style="text-align: right;">180</td>
<td style="text-align: right;">15</td>
<td style="text-align: right;">3200</td>
<td style="text-align: right;">2200</td>
<td style="text-align: right;">1200</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">1100</td>
<td style="text-align: right;">160</td>
<td style="text-align: right;">1100</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">1100</td>
</tr>
</tbody>
</table>

#### In this data, we use these nameing conventions for the column names.

For example, in “V1\_0Finfish”:

-   “V”… refers to the variable represented in the column (here V =
    “Value”)

-   …“1”… refers to the category iteration (here, = Finfish)

-   …"\_"… is simply a seperator in the title

-   …“0”.. refers to the total of the specific category.

-   …“Finfish” is purely descriptive (here the name of the category), so
    you can follow along with what is happening!

Similarly for “Q2\_2Clam”:

-   “Q”… refers to the variable represented in the column (here Q =
    “Quantity”)

-   …“2”… refers to the category iteration (here, = Shellfish)

-   …"\_"… is simply a seperator in the title

-   …“2”.. refers to the iteration of the species, such that this
    organism happens to be the second species of this category.

-   …“Clams” is purely descriptive (here the name of the species), so
    you can follow along with what is happening!

\#\#\#Lets get started

\#\#\#Caluclate Category and Entier Fishery Sums of *V* and *Q*

Ways to work your analysis
==========================

Simple Sum
----------

Calculate the simple sum of fisheries quantity from species’ quantities,
you simply sum all of the species from the entire commerical fishing
sector. No data is removed from the origional dataset for incompleteness
(as it will in the two other methods).

*C**o**m**m**e**r**i**c**a**l**F**i**s**h**i**n**g* = ∑<sub>*t* = 1</sub>(*C**o**d*, *L**o**b**s**t**e**r*, *S**e**a**w**e**e**d*, *F**l**o**u**n**d**e**r*, ...)

Quantity Method
---------------

This method works directly from the quantity data so it is good for when
*Q*<sub>*t*, *i*, *s*</sub> is often available.

I won’t get to deep in the math here – we can review these later if
needed in the discussion – but the main takeaway is that this method
simply uses the available quantity data at the species level to develop
revenue-share weighed quantity changes.

\#\#\#At the species level:

#### Total Value of species with available Q data

For where 𝑄\_(𝑡,𝑖,𝑠) is not available to a certain threshold (say 60% of
the data is missing we call it “unavailable”), the data is simply
removed from the analysis.

$$VV\_{t,i} = \\sum^l\_{s=1}(V\_{t,i,s\_{available}})$$
\#\#\#\# Revenue-share

*R*<sub>*t*, *i*, *s*</sub> = *V*<sub>*t*, *i*, *s*</sub>/*V**V*<sub>*t*, *i*</sub>

#### Revenue-share weighted quantity changes

$$QCW\_{t,i,s} = \\frac{R\_{t,i,s}+R\_{t-1,i,s}}{Q\_{t,i,s}+Q\_{t-1,i,s}}$$

### At the fishery level:

Then we calculate the revenue share, QI, and revenue-share weighted
quantity changes at the category level, which are used at the commercial
fishery level to develop the annual quantity change and index.

#### Quantity change

These, specifically the QC, are what go into the output equation.

$$QC\_{t,i} = \\sum^l\_{s=1}(QCW\_{t,i,s})$$

#### Implicit quantity index

*Q**I*<sub>*t*, *i*</sub> = *Q**I*<sub>*t* − 1, *i*</sub> \* *e**x**p*(*Q**C*<sub>*t*, *i*</sub>)

where *Q**I*<sub>*t* = 1, *i*</sub> = 1 and then
*Q**I*<sub>*t*, *i*</sub> = *Q**I*<sub>*t*, *i*</sub>/*Q**I*<sub>*t* = *b**a**s**e**y**r*, *i*</sub>

#### Value of categories available

$$VV\_{t} = \\sum^l\_{i=1}(V\_{t,i\_{available}}$$

#### Revenue share

*R*<sub>*t*, *i*</sub> = *V*<sub>*t*, *i*</sub>/*V**V*<sub>*t*</sub>

#### Revenue share weighted quantity changes

$$QCW\_{t,i} = \\frac{R\_{t,i}+R\_{t-1,i}}{Q\_{t,i}+Q\_{t-1,i}}$$

### At the entire commerical fisheries sector level:

#### Quantity change

$$QC\_t = \\sum\_{i=1}^l(QCW\_{t,i})$$

#### Quantity index

*Q**I*<sub>*t*</sub> = *Q**I*<sub>*t* − 1</sub> \* *e**x**p*(*Q**C*<sub>*t*</sub>)

where *Q**I*<sub>*t* = 1</sub> = 1 and then
*Q**I*<sub>*t*</sub> = *Q**I*<sub>*t*</sub>/*Q**I*<sub>*t* = *b**a**s**e**y**r*</sub>

Price Method
------------

Alternitively, we have a price model method to calculate implicit
quantity. Here, on top of all the work that is done for the
Quantity-derived output, we also calculate price and use price to weigh
the revenue share.

Essential by calculating price we are developing a deflator for the
total landings values: We use the total value were
*P*<sub>*t*, *i*, *s*</sub> was available (*V**V*<sub>*t*, *i*</sub>) to
calculate *P**I*<sub>*t*, *i*</sub> and extrapolate
*Q*<sub>*t*, *i*</sub> by dividing the total value
(*V*<sub>*t*, *i*</sub>)

### At the species level:

#### Price

When I say “available here” I am asking how many values of P were we
able to calculate. As you can see here, even though there were plenty of
Q and V, they didn’t amount to many P. Even if a value of P for a
species doesn’t make the cut, that gets applied to the total value of
the category.

*P*<sub>*t*, *i*, *s*</sub> = *V*<sub>*t*, *i*, *s*</sub>/*Q*<sub>*t*, *i*, *s*</sub>

#### Total value of species

And then I follow similar steps or the category and national level.

$$V\_{t,i} = \\sum^l\_{s=1}(V\_{t,i,s})$$

#### Total value of species where P is available

$$VV\_{t,i} = \\sum^l\_{s=1} (V\_{t,i,s\_{available}})$$

#### Revenue-share

*R*<sub>*t*, *i*, *s*</sub> = *V*<sub>*t*, *i*, *s*</sub>/*V**V*<sub>*t*, *i*</sub>

#### Revenue-share weighted price changes

$$PCW\_{t,i,s} = \\frac{R\_{t,i,s}+R\_{t-1,i,s}}{2\*ln(P\_{t,i,s}/\_{t-1,i,s})}$$

### At the fishery level:

#### Value of categories available

$$VV\_t = \\sum^l\_{i=1}(V\_{t,i\_{available}})$$

#### Price change

$$PC\_{t,i} = \\sum^l\_{s=1}(PCW\_{t,i,s})$$

#### Price index

*P**I*<sub>*t*, *i*</sub> = *P**I*<sub>*t* − 1, *i*</sub> \* *e**x**p*(*P**C*<sub>*t*, *i*</sub>)

where *P**I*<sub>*t* = 1, *i*</sub> = 1 and then
*P**I*<sub>*t*, *i*</sub> = *P**I*<sub>*t*, *i*</sub>/*P**I*<sub>*t* = *b**a**s**e**y**r*, *i*</sub>

#### Implicit quantity

*Q*<sub>*t*, *i*</sub> = *V*<sub>*t*, *i*</sub>/*P**I*<sub>*t*, *i*</sub>

#### Implicit quantity index

*Q**I*<sub>*t*, *i*</sub> = *Q**I*<sub>*t*, *i*</sub>/*Q**I*<sub>*t* = *b**a**s**e**y**r*, *i*</sub>

#### Revenue share

*R*<sub>*t*, *i*</sub> = *V*<sub>*t*, *i*</sub>/*V*<sub>*t*</sub>

#### Revenue share weighted price changes

$$PCW\_{t,i} = \\frac{R\_{t,i}+R\_{t-1,i}}{2\*ln(P\_{t,i}/\_{t-1,i})}$$

### At the entire commerical fisheries sector level:

#### Price change

$$PC\_t = \\sum^l\_{i=1} (PCW\_{t,i})$$

#### Price index

*P**I*<sub>*t*</sub> = *P**I*<sub>*t* − 1</sub> \* *e**x**p*(*P**C*<sub>*t*</sub>)

where *P**I*<sub>*t* = 1</sub> = 1 and then
*P**I*<sub>*t*</sub> = *P**I*<sub>*t*</sub>/*P**I*<sub>*t* = *b**a**s**e**y**r*</sub>

#### Implicit quantity

*Q**I*<sub>*t*</sub> = *V*<sub>*t*</sub>/*P**I*<sub>*t* = *b**a**s**e**y**r*</sub>

#### Implicit quantity index

*Q**I*<sub>*t*</sub> = *Q**I*<sub>*t* − 1</sub> \* *e**x**p*(*Q**C*<sub>*t*</sub>)

where *Q**I*<sub>*t* = 1</sub> = 1 and then
*Q**I*<sub>*t*</sub> = *Q**I*<sub>*t*</sub>/*Q**I*<sub>*t* = *b**a**s**e**y**r*</sub>

#### Quantity change

Same as before, these are the values that would go into the output
portion of the output equation. This method is good for data that are
missing many of the quantity values.

*Q**C*<sub>*t*</sub> = *l**n*(*Q**I*<sub>*t*</sub>/*Q**I*<sub>*t* − 1</sub>)

Commercial fisheries data availability, issues, and mitigation
==============================================================

-   How should we deal with missing data?

-   How much of the timeseries should be assessed?

-   How should species data be categorized?

Missing data
------------

NA in the commercial fisheries dataset does not mean 0, but rather that
the data may be confidential (following the rule of 3) or simply be
missing. This can be a serious issue here, as missing data could lead to
artificially large price (*P**C*<sub>*t*</sub>) and quantity
(*Q**C*<sub>*t*</sub>) changes for years in the timeseries.

There are a lot of NAs in this dataset. Some data columns are completely
filled with NA and even those that are not – So first thing we did was
to take care of columns that were mostly made of NAs. We instituted a %
missing data threshold. Here, these columns have too few data according
to a 40% threshold we’ve instituted, so we are simply going to remove
that data. Honestly, what could data with that much missing really tell
us and at what point are we just making the data up to make up for what
is missing?

Now with those offending columns of missing data gone, we can go after
the loose, infrequent, NAs. Here we impute the values from the closest
value and harkening back to our previous example, the fictitious code
value data looks a lot more realistic!

When we apply these practices for missing data to real data examples, we
see that the removal of nearly 400 species data results in a plot for
quantity index (one of our targeted end products) almost the same to one
where no data was removed. This provides evidence that those removed
data weren’t really contributing much to the results. This is also a
large dataset such that the impact of the data removed (35% of the
original data) is cushioned by how much data is remaining.

On the other hand, the removal of 58 species (approximately 25% of the
original data) radically changes this regional plot. The y-axis is
displaying beyond-reasoble values and the spike in the “Other” category
can’t possibly be correct. With the percent missing threshold
implemented, QI values appear to be in a much more sensible range.

Timeseries Reporting Consistency
--------------------------------

On to our next issue: Consistent reporting throughout the timeseries.
Looking simply at the summed quantity for each category and the entire
fishery, there have been several periods of improved reporting, such
that the increasing trend is so steep and is not indicative of real
increases in the quantity of fish caught from 1950 (when data was first
started to be collected) to today (2017). If we just take the last part
of that timeline, the trend seems more level and reasonable.

If we look at the quantity index result, we see that much of our missing
data is pre-1990 and our analysis inherently removes less data when we
subset, giving us more species data to work with.

Defining Species Categories
---------------------------

The next question is something we are still thinking about: How to
define our species categories. These can be specific or broad?

Theoretically, categories should group species with similar economic
impact (e.g., fishing costs) which can be difficult to define.

It is possible that we might be able to use taxonomic group as a proxy
for this since species in the same taxonomic group are more likely to be
caught in similar ways (an idea that is very pleasing to the biologist
in me!).

More specifically, we applied two methods:

1.  We used the same species groupings as were used in Fisheries
    Economics of the US report. This could work because there is a
    precedent for using this species split up, but it is fairly
    over-generalized. “Shellfish” is not really the same as saying “all
    invertebrates”, for example.

2.  Alternatively, thanks to renewed data managing efforts done by ST1,
    we now have ITSN numbers associated with each species, and with some
    fancy footwork, can resort these species into a variety of
    taxonomically-relevant groups.

However, with the more categories we have, the less data we have for
each category.

These plots were created using the same data, just by splitting the
categories up differently. We can see that the QI is increasing in the
first plot using the FEUS categories and that the second plot using the
taxomcially defined species has species increasing and decreasing.

This may be a key to better seeing what is actually going on in the
data.

NOAA READ ME
============

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.
