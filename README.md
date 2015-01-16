surfaceProbability
==================
The files in this repository were used to create surface probability maps for the [NetAssessApp](https://github.com/LADCO/NetAssessApp).

# Maps
The maps give an estimate of the probability of pollution levels exceeding a threshold in a given year. For example, the plot below shows the estimated probability for areas in the contiguous US exceeding ground level ozone levels of 75 ppb at least one day in a year (8-hour maximum):

![Image](https://raw.githubusercontent.com/LADCO/NetAssessApp/eric/www/images/o3_75.png)
![legend](https://raw.githubusercontent.com/LADCO/NetAssessApp/eric/www/images/probLegend.png)

<br></br>

# Data and Methods
The maps were made using [EPA/CDC downscaler data](http://www.epa.gov/nerlesd1/land-sci/lcb/lcb_faqsd.html). The files in this repository titled ```downloadDownscaler.R``` and ```mergeDownscaler.R``` contain ```R``` code for downloading data from the EPA website and and merging it with data obtained from the [CDC's Environmental Public Health Tracking program](http://ephtracking.cdc.gov/showHome.action). Downscaler data are daily estimates of ground level ozone and PM2.5 for every census tract in the contiguous US, 2007-2011. These are statistical estimates from "fusing" photochemical modeling data and ambient monitoring data using Bayesian space-time methods.

The probability estimates for the maps were created by fitting extreme value distributions to the downscaler data for each census tract. The file titled ```fitEVD.R``` contains ```R``` code for fitting the distributions using the ```rlarg.fit()``` function from the [```ismev``` package](http://cran.r-project.org/web/packages/ismev/index.html). The file ```makeMap.R``` contains code for creating the map images that are used in the NetAssessApp.

# Full Documentation
For full documentation of how the maps were generated, visit the repository GitHub page at [http://natebyers.github.io/surfaceProbability/](http://natebyers.github.io/surfaceProbability/).

