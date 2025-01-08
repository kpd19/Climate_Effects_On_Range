# Summary

This library contains code used in the manuscript titled Climate Effects of Range of the Douglas-fir Tussock Moth. This paper uses field and citizen science collected data to explore the effects of habitat and weather features on the range of populations of the Douglas-fir tussock moth. The model and analysis are implemented in R and Python. 

# Requirements and Setup

The code was built using R version 4.3.2 and Python version 3.10.12.  R can be downloaded [here](https://www.r-project.org). Python can be operated using the Jupyter Notebook package in Visual Studio Code Editor, which can be downloaded [here](https://code.visualstudio.com/download). The code requires several packages that are not part of the base R installations. After installing R, navigate to the main repository directory and run the installation.R script to install necessary packages. After installing Python, run the installation.ipynb script to install necessary packages. 

# Aggregating spatio-temporal Douglas-fir tussock moth population data 

The population data comes from several sources collected by forest managers, lab collections, and citizen science projects. The `inaturalist.R` script in the `population_data` directory aggregates the citizen science data from iNaturalist as well as lab collections collected by past and present members of the Dwyer Lab at the University of Chicago. The `defoliation.R` script in the directory aggreates the defoliation data. To deal with defoliation polygons varying in size, we spatially aggregating defoliation polygons within the same year. Next, for polygons greater than or equal to 9 square kilometers, we split each polygon into sub-polygons with an average size of 3 square kilometers. Finally, we took the centroid of each polygon to use in our analyses. We used synthetic data as *pseudo-absences* in our model, which are generated in `nearby_coords.R`. We used the 0.25&deg; latitude x 0.25&deg; longitude grids from the weather data to randomly sample 3 population points in each grid square. These sources of present and pseudo absent data are aggreaged in `combine_sources.R`. 

# Habitat and Weather Features

We use several markers of habitat and weather quality to analyze the effects on population range, including biomass, forest composition, and landscape type. Biomass for the contiguous United States can be downloaded from [here](https://data.fs.usda.gov/geodata/rastergateway/biomass/), biomass for Canada can be downloaded from [here](https://open.canada.ca/data/en/dataset/698dc612-5059-43ee-84f3-49756e6d5ad6). The script `biomass.R` in the `landscape` directory shows how average and maximum biomass within a 3 kilometer radius around each location. We have provided an example file `range_locations_example.csv` in the `data` directory to use for analyses because the entire dataset takes a long time to extract the data.  For the Canadian biomass dataset, the data product is not masked by forest cover, so we used the same data to mask the Canadian biomass output. The forest cover dataset can be downloaded [here](https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/download.html). The Canadian dataset also records the biomass as t/ha, whereas the United States dataset is Mg/ha, so the Canadian dataset is converted to Mg/ha. An example of the output is in the `data` directory as `biomass_all_sites_example.R`.

# Model fitting and comparison

# Making projections under climate change

