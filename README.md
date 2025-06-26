chlorofluoroR <img src="man/figures/logo.png" align="right" height="138" /></a>
======================================================================

An `R` package for in-house analysis of chlorophyll fluorescence data from 
FluorCam FC800-331 and FluorCam 7 Software. Based on wet-lab 
[protocol](https://www.protocols.io/view/non-photochemical-quenching-npq-relaxation-analysi-eq2lyw7mmvx9/v1) 
from Long Lab. This package was compiled primarily to aid plant selection, 
hence, it not only plots the CF results but overlaps them with copy number data
and herbicide selection (ie. BASTA) results. 

<br> 


Author
======================================================================
Katie Jeynes-Cupper, University of Illinois Urbana-Champaign, kejc@illinois.edu


Table of Contents
======================================================================
-   [Overview](#Overview)
-   [Installation](#installation)
-   [Input files](#Input-files)
-   [Quick Start](#Quick-Start)
-   [Output](#Output)
<br>

Overview
======================================================================

**chlorofluoroR** is an `R` package that provides a pipeline for quick 
processing and plotting of CF imaging data for specific parameters. Here, 
we wish to include/overlap with copy number data (based on BAR tag) and BASTA 
selection. Get data analysed in less than 30 mins :) 


<br> 

Installation 
======================================================================
The latest version of the package can be install directly from this
GitHub repo:

``` r

if (!require("devtools")) install.packages("devtools")
devtools::install_github("KJeynesCupper/chlorofluoroR", ref = "main")

```

Load package into R library:
``` r

library(chlorofluoroR)

```
<br>

Input files
======================================================================
See examples of format of input files:

1. Plate layout 
2. Copy number results
3. Data from FluorCam software (frames-numeric)
4. BASTA selection (smartsheets log)

Quick Start
======================================================================
**chlorofluoroR** utilizes 96-well plate data. This data can be inputted as a 
single dataframe or a list of dataframe (each representing an individual plate). 

The output data from the FluorCam software requires some wrangling - we can
help with that. 

<br>

## 1. Load data into R
First we must load our:
- plate layouts
- copy number results
- Data from FluorCam software (frames-numeric)
- BASTA selection

DO NOT RUN THIS CODE
```r 
data("CF_demodata")
data("layout")
data("selection_results")
data("copynumber")
output_location = tempdir()
plate_names = names(CF_demodata)
smartsheet = selection_results


```
<br>

## 2. Computing the FvFm and PSII distribution

This function wrangles the raw data from the FluorCam software, and organises
it into a nice excel spreadsheet. This excel doc will have separate sheets for 
the FvFm and PSII data - these will be specific for each plate.

```r
step_one <- computeFvFm_PSII(CF_demodata, 
                              plate_names, 
                              layout, 
                              smartsheet,
                              copynumber,
                              output_location)
```
This will return a list of dataframes, where each plates has a dataframe for
FvFm and PSII. And saves an excel file containing all data 
(chlorofluoro_dataset_1.xlsx)and pdf plot for the FvFm and PSII 
(chlorofluoro_plot_1.pdf). The plot is a line graph with SD at each timepoint for 
each plant sample. The colour of each sample is determined by the BASTA selection
results, where green represents positive (ie. resistant) and red represents 
negative (ie. not resistant). The line type is determined by the copy number 


<br>

## 2. Subset for specific combinations

We want to use this data to help us select for lines to carry to T2. So, here
we will filter for these desired traits. For example, we will select for those
with a copy number value of 4 and BASTA positive (ie. resistant). 

``` r
data("step_one")
step_two <- subset_chlorofluoro(step_one, output_location)


```
This outputs a plot and excel document in the same way as compute_chlorofluor() 
function. See output as files "chlorofluoro_plot_2.pdf" and 
"chlorofluoro_dataset_2.xlsx"
<br>


## 3. Summarise the FvFm and PSII values for each plant line. 
Instead of looking at the distribution of the FvFm or PSII over multiple light
pulses across time, we can calculate a mean. Here we plot the mean for these 
variables as a bar plot with error bars representing SD. As before, the colour 
is determined by the BASTA selection result. Output is also saved as an excel 
document (one sheet per plate and variable). See output as files 
"chlorofluoro_plot_3.pdf" and "chlorofluoro_dataset_3.xlsx"

``` r
data("step_one")
step_three <- summarise_chlorofluoro(step_one, output_location)

```

<br>

Output
======================================================================
Each of the three functions in this package outputs either an excel file or a 
pdf or BOTH! 

At the end, you will likely have these files: 
- chlorofluoro_dataset_1.xlsx
- chlorofluoro_dataset_2.xlsx
- chlorofluoro_dataset_3.xlsx

- chlorofluoro_plot_1.pdf
- chlorofluoro_plot_2.pdf
- chlorofluoro_plot_3.pdf

Here is breakdown of each. 

**chlorofluoro_dataset_1.xlsx**
Contains the columns:
- `Plant_ID`
- `Replicate`
- `Selection`
- `Timepoint`
- `Time`
- `BAR_copy`
- `FvFm`
  or `PhiPSII`

**chlorofluoro_dataset_2.xlsx**
Contains the columns:
- `plate`
- `Plant_ID`
- `Replicate`
- `Selection`
- `Timepoint`
- `Time`
- `BAR_copy`
- `FvFm`
  or `PhiPSII`

**chlorofluoro_dataset_3.xlsx**
Contains the columns:
- `Plant_ID`
- `techreps`
- `Selection`
- `FvFm_Mean`
  or `PSII_Mean`
- `FvFm_SD`
  or `PSII_SD`
  
Description of plots:
- chlorofluoro_plot_1.pdf: Chlorophyll fluorescence parameters Fv/Fm and ΦPSII
across genotypes and transgene copy number.Line plots depict changes in (left) 
the maximum quantum efficiency of PSII (Fv/Fm) and (right) the effective quantum 
yield of PSII (ΦPSII) over time (minutes). Each line represents the mean value 
for an individual plant (distinguished by color), grouped by Plant ID. 
Colors represent BASTA selection result (green/positive or red/negative).
Line types denote transgene copy number (e.g., 0, 1, 2). Error bars represent 
the standard error of the mean (SEM) at each time point. 

- chlorofluoro_plot_2.pdf:Chlorophyll fluorescence parameters Fv/Fm and ΦPSII
across specific genotypes and/or transgene copy number. Line plots depict 
changes in the maximum quantum efficiency of PSII (Fv/Fm) and the effective 
quantum yield of PSII (ΦPSII) over time (minutes). Each line represents the mean 
value for an individual plant (distinguished by color), grouped by Plant ID. 
Colors represent BASTA selection result (green/positive or red/negative).
Line types denote transgene copy number (e.g., 0, 1, 2). Error bars represent 
the standard error of the mean (SEM) at each time point. 

- chlorofluoro_plot_3.pdf:Summary of chlorophyll fluorescence parameters Fv/Fm 
and ΦPSII across individual plants.Bar plots show the mean values of (left) 
Fv/Fm (maximum quantum efficiency of PSII) and (right) ΦPSII for each plant. 
Each bar represents the average measurement for an individual plant, 
with error bars indicating the standard deviation (SD). Colors are assigned 
based on the selection marker results (green/positive or red/negative). 


<br>
*Last updated:* 06-25-2025
