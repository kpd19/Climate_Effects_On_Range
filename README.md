# Summary

This library contains code used in the manuscript titled Climate Effects of Range of the Douglas-fir Tussock Moth. This paper uses field and citizen science collected data to explore the effects of habitat and weather features on the range of populations of the Douglas-fir tussock moth. The model and analysis are implemented in R and Python. 

# Requirements and Setup

The code was built using R version 4.3.2 and Python version 3.10.12.  R can be downloaded [here](https://www.r-project.org). Python can be operated using the Jupyter Notebook package in Visual Studio Code Editor, which can be downloaded [here](https://code.visualstudio.com/download). The code requires several packages that are not part of the base R installations. After installing R, navigate to the main repository directory and run the installation.R script to install necessary packages. After installing Python, run the installation.ipynb script to install necessary packages. 

# Aggregating spatio-temporal Douglas-fir tussock moth population data 

The population data comes from several sources collected by forest managers, lab collections, and citizen science projects. The `inaturalist.R` script in the `population_data` directory aggregates the citizen science data from iNaturalist as well as lab collections collected by past and present members of the Dwyer Lab at the University of Chicago. The `defoliation.R` script in the directory aggreates the defoliation data, which was downloaded from the [Annual Insect & Disease Detection Survey Database](https://www.fs.usda.gov/science-technology/data-tools-products/fhp-mapping-reporting/detection-surveys). To deal with defoliation polygons varying in size, we spatially aggregating defoliation polygons within the same year. Next, for polygons greater than or equal to 9 square kilometers, we split each polygon into sub-polygons with an average size of 3 square kilometers. Finally, we took the centroid of each polygon to use in our analyses. We used synthetic data as *pseudo-absences* in our model, which are generated in `nearby_coords.R`. We used the 0.25&deg; latitude x 0.25&deg; longitude grids from the weather data to randomly sample 3 population points in each grid square. These sources of present and pseudo absent data are aggreaged in `combine_sources.R`. 

# Habitat features

We use several markers of habitat and weather quality to analyze the effects on population range, including biomass, forest composition, and landscape type. Biomass for the contiguous United States can be downloaded from [here](https://data.fs.usda.gov/geodata/rastergateway/biomass/), biomass for Canada can be downloaded from [here](https://open.canada.ca/data/en/dataset/698dc612-5059-43ee-84f3-49756e6d5ad6). The script `biomass.R` in the `landscape` directory shows how average and maximum biomass within a 3 kilometer radius around each location. We have provided an example file `range_locations_example.csv` in the `data` directory to use for analyses because the entire dataset takes a long time to extract the data.  For the Canadian biomass dataset, the data product is not masked by forest cover, so we used the same data to mask the Canadian biomass output. The forest cover dataset can be downloaded [here](https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/download.html). The Canadian dataset also records the biomass as t/ha, whereas the United States dataset is Mg/ha, so the Canadian dataset is converted to Mg/ha. An example of the output is in the `data` directory as `biomass_all_sites_example.R`.

The next feature of habitat quality uses the land cover classification dataset from the North American Land Change Monitoring System (download [here](http://www.cec.org/north-american-environmental-atlas/land-cover-2010-modis-250m/)), to characterize the landscape at each site. The script `modis.R` in the `landscape` directory collects the land cover classification at each site, as well as the percent of each land cover class within 5 kilometers. During the first step, we identify locations in our population dataset that are not suitable for use in our random forest model, the majority of which are in residential or cropland areas. We exclude these locations because the biomass and forest composition datasets are not reliable in these areas. To further identify Douglas-fir tussock moth habitat, we also generate a feature `near_needle` which is 1 if the location is within 5km of a needleleaf forest class, and 0 if not within 5km. 

The script `forest.R` in the `landscape` directory uses rasters on forest composition to aggregate percent of each tree species within 3 kilometers of each location. The data for the continental United States can be found [here](https://data.fs.usda.gov/geodata/rastergateway/forest_type/). Detailes for the data for Canada can be found [here](https://gee-community-catalog.org/projects/ca_species/), which is accessed through Google Earth Engine. You must first create a Google Earth Engine [project](https://code.earthengine.google.com/register) before you can access the data. The example code used in Google Earth Engine can be found as `Earth_Engine_CA_TREE_example.txt`, although it must be executed in Google Earth Engine. The example script and output of forest composition can be found in `data`.

The `elevation.R` script in the `landscape` directory uses the `geonames` package to aggregate the elevation at each location. You must first create an account [here](https://www.geonames.org/login), and then enter your account where it says "your_username". Even the example data, which only includes 1000 locations, takes a long time to run and occasionally crashed because it collects each elevation individually. To ameliorate this, the script loops through subsets of the dataset before aggregating them all at the end. The example output on the example data is included in data as `elevation_example.R`. 

The script `combine_features.R` aggregates the biomass, land cover class, forest composition, and elevation features. The example output for the subset of example locations is in the `data` directory as `all_habitat_features.csv.

# Weather Features

The historical weather data from used in our analyses comes from ECMWF Reanalysis v5 (ERA5), which has data from 1940- present day. The weather data can be accessed from the Climate Data store via their API. Details for getting an account and downloading the data can be found in the script `Download_ERA_5_CDS.ipynb` in the `weather` directory. The script is written to be executed in a [Google Colab](https://colab.research.google.com/) environment. The historical weather reanalysis data downloaded includeds hourly temperature, humidity, and precipitation from 1940-2023. The data is cleaned in the scripts in the `weather` directory. The data is then resampled from daily to the following weather summary statistics:

* Average monthly mean temperature
* Average monthly relative humidity
* Monthly total precipitation
* Annual total precipitation
* Date of predicted hatch, based on the literature using the sum of degree days above 5.6&deg;C.
* Summed degree days for the 70 days after the predicted hatch date

The climate change projections come from the [NASA Earth Exchange Global Daily Downscaled Projections](https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6) (NEX-GDDP-CMIP6). The data is then downsampled from hourly to daily timescales using the `CMIP6_data_cleaning.ipynb` and the same summary statistics as the historical data are calculated. 

# Model fitting and comparison

# Making projections under climate change

