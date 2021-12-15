This readme.txt file was generated on 2021-12-15 by Maria Paniw


GENERAL INFORMATION

1. Title of Dataset: Data from: Higher temperature extremes exacerbate negative disease effects in a social mammal.

2. Author Information
	A. Principal Investigator Contact Information
		Name: Maria Paniw
		Institution: Doñana Biological Station (EBD-CSIC)
		Address: Seville, 41001 Spain
		Email: m.paniw@gmail.com

	B. Associate or Co-investigator Contact Information
		Name: Tim Clutton-Brock
		Institution: University of Cambridge
		Address: Downing Street, Cambridge, UK
		Email: thcb@cam.ac.uk 


3. Date of data collection:(1997-01-01 until 2019-01-01 for demographic data and Tuberculosis data) 

4. Geographic location of data collection: Kuruman River Reserve, South Africa (KRR, 26°58’S, 21°49’E)

5. Information about funding sources that supported the collection of the data: 

European Research Council Advanced Grant (No 742808 and No 294494); MAVA Foundation


SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: license CC0 1.0

2. Links to publications that cite or use the data: NA

3. Links to other publicly accessible locations of the data: NA

4. Links/relationships to ancillary data sets: NA

5. Was data derived from another source? NO

6. Recommended citation for this dataset: 

Paniw, M., de la Riva, E. G., Lloret, F. (2021). Data from: Demographic traits improve predictions of spatiotemporal changes in community resilience to drought. Zenodo. https://doi.org/10.5061/dryad.xwdbrv1cn


DATA & FILE OVERVIEW

1. File List: 

funcTraits.csv: species-specific functional traits for shrubs
demoTraits.csv: species-specific demographic traits for shrubs 
speciesCover.csv: proportional cover of shrub species in 18 plots across 8 time periods 
plotCover.csv: total plant cover per plot 
seed_flower.csv: raw data to calculate minimum size at flowering (considered a demographic trait) 

plot_pca.R: R script to plot distributions of community-weighted demographic and functional traits in PCA space 

resilience_analysis_traits.R: R script to analyse different community resilience measures as functions of individual community weighted demographic or functional traits

plot_resilience_traits.R: R script to plot different community resilience measures as functions of individual community weighted demographic or functional traits

resilience_analysis_PCA.R: R script to analyse and plot different community resilience measures as functions of PCA space describing community weighted demographic or functional traits jointly

resilience_analysis_PCA_alternative.R: same as above but allowing for more PCA axes to be modelled

2. Relationship between files, if important: The .csv files are necessary to run the .R scripts

3. Additional related data collected that was not included in the current data package: no 

4. Are there multiple versions of the dataset? No
	A. If yes, name of file(s) that was updated: 
		i. Why was the file updated? 
		ii. When was the file updated? 


METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: 

Quantifying community composition

In order to assess community composition before, during, and after a severe drought event in DoÃ±ana National Park, 18 permanent plots of 25 m2 (5 Ã— 5 m) were established in November 2007 (two years after the drought) on a gradient of drought impact. The plots were located at three sites (with six plots per site): Raposo (N 37Âº0â€²2â€³, W 6Âº30â€²20â€³; at 18 m a.s.l.), MarquÃ©s (N 37Âº0â€²45â€³, W 6Âº31â€²50â€³; 21 m a.s.l), and Ojillo (N 36Âº59â€²40â€³, W 6Âº30â€²50â€³; 30 m a.s.l.). To avoid spatial autocorrelation, all plots were separated by at least 50 m from each other. Species plant cover was estimated from contacts with branches along transects within plots; these contacts were divided into two categories corresponding to living or dead canopy. Dry organs with signs of old decay (stumps, decomposed stems, branches without thin tips) were excluded. Thus, canopy prior to the episode was considered as the sum of living and dead (i.e., dry) plant canopy in 2007. Relative abundance of each species per plot in years after the extreme drought was calculated as the proportion of their contacts of living canopy relative to the sum of the contacts of living canopy of all species. Similarly, the total vegetation cover per plot was calculated as the summed contacts of living canopy of all species. Relative abundances previous to the drought were calculated as the sum of the living and dead canopy of 2007 of each species relative to the sum of the living and dead canopy of 2007 for all species.

Demographic traits

We obtained four demographic traits for each plant species encountered in the sampling plots: age at first reproduction (AgeFR), maximum longevity (L), minimum size at first reproduction (SizeFR), and ratio of adults to recruits (R/A). To calculate the first three traits for each species, 50 plants growing near the plots were randomly selected in June, 2019, in vegetation patches without signs of drought-induced impact. We estimated plant age from yearly growth scars in the main shoots. We calculated age at first reproduction from inverse prediction (Probability of not flowering = 0.9) obtained from the logistic nominal regression between reproduction stage and plant age following the standard procedure. We obtained minimum size at first reproduction using receiver operating characteristic (ROC) curves. Lastly, we approximated maximum longevity as the 90th percentile of estimated age from yearly growth scales in the 50 plants growing near the plots in patches not impacted by drought; since scars disappear in older plants, 5 to 10 years age intervals - depending on speciesâ€™ growth form and size - were added to the older scars in larger plants. We calculated ratios of recruits to adults (R/A) from data collected in the years 2007, 2008, 2013 and 2019. Data included the total number of juvenile non-reproductive plants (recruits) and the total number of adults of each species found in plots with low drought impact (6 plots).

Functional traits

We calculated community-weighted means of eight above- and two below-ground traits, which are among the most commonly used to describe plant functional types and have previously been obtained in the study plots in late spring 2013. Above-ground functional traits included plant height (Phg), leaf area per unit of leaf dry mass (SLA), leaf dry matter content (LDMC), stem dry matter content (SDMC), leaf nitrogen concentration (LNC), leaf chlorophyll concentration (LChl), isotopic carbon fraction (Î´13C), and seed mass (Smass). Below-ground traits included specific root area (SRA) and root dry matter content (RDMC).


2. Methods for processing the data: 

All data were processed in R; for functional traits, see additionally https://doi.org/10.1093/jpe/rtw027

3. Instrument- or software-specific information needed to interpret the data: 

R statistical software, version 4.0.0 (and packages as described in the R scripts)

4. Standards and calibration information, if appropriate: 

5. Environmental/experimental conditions: NA

6. Describe any quality-assurance procedures performed on the data: All R scripts are fully commented and have been checked; all raw data was quality-checked by F. Lloret and E. De la Riva.

7. People involved with sample collection, processing, analysis and/or submission: I. Granzow, J. Margalef, M. Angeles PÃ©rez, and T. Quirante, M. Paniw, F. Lloret, E. De la Riva 


DATA-SPECIFIC INFORMATION FOR: [funcTraits.csv]

1. Number of variables: 11

2. Number of cases/rows: 11

3. Variable List: 

Species: Latin species name
LDMC: Leaf dry matter content
leaf_chlo: Leaf chlorophyll
SLA: specific leaf area
N_leaf: Leaf nitrogen content
C13: Isotopic carbon fraction
SRA_root: specific root area
RDMC: Root dry matter content
Seed size: seed mass
Height: plant height
dry_matter: stem dry matter content  

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no


DATA-SPECIFIC INFORMATION FOR: [demoTraits.csv]

1. Number of variables: 3

2. Number of cases/rows: 11

3. Variable List: 

Species: Latin species name
ageFirstRep: plant age at first reproduction 
Longevity90p: plant longevity
r_a_ratio: ratio of recruits to adults

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no


DATA-SPECIFIC INFORMATION FOR: [speciesCover.csv]

1. Number of variables: 11

2. Number of cases/rows: 126

3. Variable List: 

Plot: one of 18 plots pre drought and in years following drought
Remaining variables are Latin species names

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no


DATA-SPECIFIC INFORMATION FOR: [plotCover.csv]

1. Number of variables: 11

2. Number of cases/rows: 18

3. Variable List: 

Plot: plot name
dieOffCat: severity of drought die off
Total die-off: percentage die off of cover after drought

Remaining columns depict total cover across different time periods

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no


DATA-SPECIFIC INFORMATION FOR: [seed_flower.csv]

1. Number of variables: 6

2. Number of cases/rows: 553

3. Variable List: 

Diameter(cm): plant diameter in cm
H (cm): plant height in cm
Flower: flowering (Y) or not (N)
Green(%): percent green tissue on plant
caAge (y): estimate of plant age
Species: species name

Remaining columns depict total cover across different time periods

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no
