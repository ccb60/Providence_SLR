# Providence SLR Analysis

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />
    
Analysis of sea level rise data from Providence, RI

# Introduction
This project focuses on analyzing sea level rise trends in Providence, RI.  
The primary purpose of the archive was to extend analysis performed in Casco Bay 
to another location.


# SLR Trend Data
The data used to produce the SLR graphic was downloaded directly from NOAA
Tides and Currents here:
[Providence SLR Info from NOAA](https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8454000).

The data description on the source web site says the following: 
> "[The data] shows the monthly mean sea level without the regular seasonal
  fluctuations due to coastal ocean temperatures, salinities, winds, atmospheric
  pressures, and ocean currents. ... The [data] values are relative to the most
  recent Mean Sea Level datum established by CO-OPS."

In other words, these data are not raw data, but have been pre-processed by
NOAA to remove seasonal patterns. The webpage declares the average Sea Level
Rise to be 2.41 +/- 0.24 mm/yr. We confirmed that result in our own analyses of 
these data.

# Related Observational Data
Related observational monthly data, was downloaded via a NOAA API using a
simple python script
[included in this archive](Original_Data/providence_tide_gage_monthly.py).
Data from the two data sources is highly correlated, but not identical, which
presumably reflects NOAA's pre-processing of the data.

Hourly data was downloaded via the NOAA API to study probability of
daily tidal elevations exceeding a flood level equivalent to the current
"Highest Astronomical Tide" level for the Providence gage.  (Although
the HAT elevation for each tide station is available electronically, 
it is quick to look up by hand.  For Providence it is 6.52 feet MLLW,
or 1.987 meters MLLW.

Providence's Tidal Epoch is the same as for Portland, Maine:  1983 - 2001.
