# README File
Code submitted with Pisor et al. to One Earth.<br>
Scripts written by Danielle Touma (detouma@ucar.edu).<br>
This is an example workflow of identifying and characterizing climate extremes in an observational dataset, processing adaptation data, and taking a first step in relating the two datasets.<br>

# Notes
 - Script will download data locally to your machine, but is adaptable to work in the cloud or on a cluster.<br>
 - Depending on the datasets, you may require ~80GB of RAM. This can be greatly reduced by subsetting for space or time, or using a coarser resolution dataset. <br>
 - find_runs.py is an external script that needs to be in the working directory. It can be downloaded from: https://gist.github.com/alimanfoo/c5977e87111abe8127453b21204c1065. <br>

# Workflow for example: <br>
1. download CHIRPS precipitation data and calculate percentile thresholds by running download_pr_and_calculate_percentiles.ipynb. <br>
(More information about CHIRPS can be found here: https://www.chc.ucsb.edu/data/chirps).
2. run adaptation_jupyter_notebook.ipynb to process climate and remittance data and create Figures 1 and 3. Remittance data will be downloaded within this script.


