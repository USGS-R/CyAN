## Introduction

This app is used to map, download, and visualize CyAN data. Most functions of the app require a connection to a CyAN database file. Until the data release accompanying this app, if you are a CyAN team member or reviewer, you can get a copy of the database by contacting Patrick Eslick <peslick@usgs.gov>. 

The rest of this page explains how to use the app. Each heading represents a menu item from the sidebar. The sidebar menu can be opened or closed at any time by clicking the icon next to "CyAN" in the upper left corner.

To run the app, you can install the CyAN package, load the package using `library()` and then use the `run_CyAN()` function. In an R session you can run the following code:

```r
devtools::install_github("PatrickEslick/CyAN")
library(CyAN)
run_CyAN()
```
The app should launch in your default web browser. The app was developed using the Google Chrome web browser.

## Configure

To use most of the functions of the app, you will need to connect the app to your copy of the CyAN database. Click the "Database file" button and navigate to the CyAN.db file (see Introduction for information on getting this database). 

## Map

### Viewing data points

You can filter which data points appear on the map using the panel in the upper right corner of the map screen. Points will be shown when you click the "Show points" button. Only points in your current zoom level will be shown on the map. Plotting many points at once can make the map screen run slowly, so it may be smoother to zoom into your region of interest first before showing the points on the map.

You can also change the base layer from this panel. Available layers include the default street map, NHD, and LandsatLook. Some layers may not show up at certain zoom levels.

### Downloading data

Don't try to download the entire data set at once. Zoom to a region of interest or limit to a narrow range of years and/or parameters of interest. If you are interested in a larger data set, try using some of the R functions available in this package, especially `get_cyan_data()`. See `help(get_cyan_data)` for details.

To download data, type a file name in the box, make sure your map bounds are filled in (or manually enter your own bounding box), fill in any additional search parameters, and click "Download". A notification will pop up in the lower right corner indicating that the data query is working. The "Add GMT datetime" option will attempt to add a column of GMT time to the output file. Some data points that have missing or ambiguous time zones will not have this column filled in. The "Add solar noon flag" option will add two columns to the output. One indicating whether the sample was taken in solar noon (1000 - 1400) local time, and one indicating whether the sample was taken in extended solar noon (0900 - 1500).

## Bivariate Plot

The bivariate plot tab allows you to plot simultaneous observations of two parameters against each other. Select two parameters from the drop down menu, and the app will automatically start searching for data. Status updates are shown in the lower right corner. If your selection returns more than 10,000 points, only the first 10,000 will be shown. A notification will let you know if that's the case. You can limit your search by map bounds or time period.

You can highlight points by method of analysis by selecting which parameter's methods you would like to highlight under "Highlight" and then selecting one or more methods from the "Methods" box. Use the "Log Scale" check boxes to transform either or both axes.

The right plot is zoomed to the selection on the left plot. To zoom, click and drag a box on the left plot.
