# README File
Code submitted with Pisor et al. to One Earth. <br>
Python scripts written by Danielle Touma (detouma@ucar.edu) and R script is written Anne Pisor (anne.pisor@wsu.edu).<br>
This is an example workflow of identifying and characterizing climate extremes in an observational dataset, processing adaptation data, and taking a first step in relating the two datasets.<br>
For more information about the rationale, methods, and findings, please refer to the paper and supplementary material.

# Notes
 - Script will download data locally to your machine, but is adaptable to work in the cloud or on a cluster.<br>
 - Depending on the datasets, you may require ~80GB of RAM. This can be greatly reduced by subsetting for space or time, or using a coarser resolution dataset. <br>
 - find_runs.py is an external script that needs to be in the working directory. It can be downloaded from: https://gist.github.com/alimanfoo/c5977e87111abe8127453b21204c1065. <br>
 - remittance dataset have been processed by the authors for ease of use. The workflow below downloads this post-processed data for the analysis from the the github repository. The original dataset can be found here: https://microdata.worldbank.org/index.php/catalog/95. The R script (Code for processing World Bank remittance data.R) along with the lat/longs coded by AP (burkina locations_meso.csv) used for processing the remittance dataset is uploaded to the github for transparency but not intended to be used with other datasets. 
 - shapefiles are downloaded from the github for ease of access, but can also be found here: www.gadm.org

# Workflow for example: <br>
1. download CHIRPS precipitation data and calculate percentile thresholds by running download_pr_and_calculate_percentiles.ipynb. <br>
(More information about CHIRPS can be found here: https://www.chc.ucsb.edu/data/chirps).
2. run adaptation_jupyter_notebook.ipynb to process climate and remittance data and create Figures 1 and 3. Remittance data will be downloaded within this script.



