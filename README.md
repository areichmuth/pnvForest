# Current and future projections of PNV formations 
Here, we model the potential natural vegetation (PNV) of forest ecosystems using the model output data from Reichmuth et al. 2025 (raster data). We model the PNV for the reference time period (1971-1990) as well as the projection period 2079-2098 for RCP 4.5 and RCP 8.5. To do so, we emulate PNV formations in reference to Bohn et al. 2003 (vector data). The vector data are available under Federal Agency for Nature Conservation (BfN) [request data](mailto:info@bfn.de)

## Running code
You need to change the paths within the files according. Please stick to the order here, as each script produces results that the following scripts depend on. The order also is visible within *start.sh*. You can uncomment and call them accordingly. 
Within the file *sourceFunctions.R* all background functions are included. Leave the structure of folders and scripts as it is.


## Normalisation
The model output by Reichmuth et al. 2025 show a zero-inflated beta distributions for the species occurrences. To aggregate the species, we need to normalise the occurrences and later create a dataframe for the following analysis. Download the data from [https://doi.org/10.48758/UFZ.18058](https://doi.org/10.48758/UFZ.18058) 

Run:
* prepare/newMax.R
* prepare/prepareData.R

## Emulation
First, we emulate PNV formations in reference to Bohn et al. 2003 using an optimisation algorithm. Within this algorithm, each tree species that is described as a dominant or sub-dominant tree species within the formation by Bohn et al. 2005 and occurrs within the model data by Reichmuth et al. 2025, is considered. For each of these species within the formation, the algorithm iteratively finds the best weight for the species occurrence probabilities to aggregate the mean over all considered species. This weighted mean of all considered species results an emulated formation. To evaluate each iteration of emulation, we apply a TSS maximisation using PNV data by Bohn et al. (2003) as reference. The algorithm returns the best weights per species within the formation as well as the TSS threshold from within the maximisation. The *applyOptim.R* algorithm applies the result from the optimisation as occurrence probability and binary maps. All is projected to the projection period as well by using the same weights.    


* optimisation/optim.R - run '''bash orchestrateOptim.sh'''
* optimisation/applyOptim.R
* optimisation/optimValues.R


## Analysis
Then we extracted the proportion which emulated formations occurr within the reference PNV extent. Here we return a map and the proportions.

* analyse/comparePnv.R
* analyse/evalPnvShp.R

We extracted the amount of binarised formations per pixel for the reference and projection period to retrieve the change over time of emulated PNV formations. Here now we take the difference to retrieve the change over time. 

* analyse/sumDiff.R


## Sources

Bohn, U., Neuhäusl, R., Gollub, G., Hettwer, C., Neuhäuslova, Z., Schlütter, H., & Weber, H. (2003). Karte der
natürlichen Vegetation Europas: Maßtab 1:2 500 000 (7 ed.). Bundesamt für Naturschutz

Reichmuth, A., Kühn, I., Schmidt, A., and Doktor, D. (2025). Forested Natura 2000 sites under climate change: effects of tree species distribution shifts, Web Ecol., 25, 59–89, [https://doi.org/10.5194/we-25-59-2025](https://doi.org/10.5194/we-25-59-2025) 

Reichmuth, A., Kühn, I., Schmidt, A., & Doktor, D. (2026). Tree species projections for 2 time periods within
1971-2098 for Europe. [https://doi.org/10.48758/UFZ.18058](https://doi.org/10.48758/UFZ.18058)
