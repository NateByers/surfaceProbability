surfaceProbability
==================
These files create surface probability maps for the [NetAssessApp](https://github.com/LADCO/NetAssessApp).

# Maps
The maps give an estimate of the probability of pollution levels exceeding a threshold in a given year. For example, the plot below shows the estimated probability for areas in the continental US exceeding ground level ozone levels of 75 ppb at least one day in a year (8-hour maximum):

![Image](https://raw.githubusercontent.com/LADCO/NetAssessApp/8e30d6505a3dbf60d2d36430fb63004d8510e30d/www/images/OzoneProb75ppb.png)

# Data and Method
The maps were made using [EPA/CDC downscaler data](http://www.epa.gov/nerlesd1/land-sci/lcb/lcb_faqsd.html). The files ```downloadDownscaler.R``` and ```mergeDownscaler.R``` download and merge data from the EPA website and data obtained from the [CDC's Environmental Public Health Tracking program](http://ephtracking.cdc.gov/showHome.action). Downscaler data are daily estimates of ground level ozone and PM2.5 for every census tract in the continental US, 2007-2011. These are statistical estimates from "fusing" photochemical modeling data and ambient monitoring data using Bayesian space-time methods.

The probability estimates for the maps were created by fitting extreme value distributions to the downscaler data for each census tract. The file ```fitEVD.R``` fits the distributions using the ```rlarg.fit()``` function from the [```ismev``` package](http://cran.r-project.org/web/packages/ismev/index.html). The file ```makeMap.R``` creates the map .png images that are used in the NetAssessApp.

