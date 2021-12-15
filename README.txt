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


3. Date of data collection: All biological data used to run the individual-based models in this work was collected 1997-01-01 until 2019-01-01 (demographic data and Tuberculosis data). Rainfall and temperature data are from 1979-2020. Future projections of temperature and rainfall are deposited at 10.6084/m9.figshare.16794001.  

4. Geographic location of data collection: Kuruman River Reserve, South Africa (KRR, 26°58’S, 21°49’E)

5. Information about funding sources that supported the collection of the data: 

For biological data: European Research Council Advanced Grant (No 742808 and No 294494); MAVA Foundation


SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: license CC0 1.0

2. Links to publications that cite or use the data: NA

3. Links to other publicly accessible locations of the data: NA

4. Links/relationships to ancillary data sets: NA

5. Was data derived from another source? Yes

	Sources (for climate data): https://esgf-node.llnl.gov/projects/cmip5/		

6. Recommended citation for this dataset: 

Paniw, M., Duncan, C., Groenewoud, F., Drewe, J.A., Manser, M., Ozgul, A., Clutton-Brock, T. (2021). Data from: Higher temperature extremes exacerbate negative disease effects in a social mammal. Zenodo. Xxxx


DATA & FILE OVERVIEW

1. File List: 

GAMS: folder containing the most parsimonious generalised additive models (GAMs - saved as R data files) describing stage-specific meerkat demographic rates. 
vitalRatesPred.pdf: Plots of predictions from the the most parsimonious GAMs
Age.mass.domM.csv: averages ages and masses of dominant meerkat males per month and year (averaged over all individuals and groups) 
init_group_comp.csv: initial composition of 10 meerkat groups used to initialise IBM simulations 
past.rain.temp.csv: standardised rainfall and mean maximum temperature deviations per month and year used to fit demographic-rate GAMs
rainfall_GPCP_1979_2020: raw rainfall values for the study site obtained from https://psl.noaa.gov/data/gridded/data.gpcp.html
tempNOAA_CPC_1979_2020: raw maximum temperature values for the study site obtained from https://psl.noaa.gov/data/gridded/data.cpc.globaltemp.html
gcm_output.csv:  projections of changes in the frequency of prolonged temperature extremes for different Global Circulation Models - where GCM outputs at the closest grid point to study location were used
gcm_output_2grid_point_interpol.csv:  projections of changes in the frequency of prolonged temperature extremes for different Global Circulation Models - where GCM outputs at the two closest grid points to study location were used (averaged) 

meerkat_group_IBM_baseline.R: Implements an individual-based model (IBM) scaling from individual and stage-specific vital rates to groups dynamics under climate TB interactions (assuming observed climate)
meerkat_group_IBM_scen_extreme.R: Implements an individual-based model (IBM) scaling from individual and stage-specific vital rates to groups dynamics under climate TB interactions (assuming increased likelihood of extremely hot years)
meerkat_gcm_risks.R: Implements risk analyses of future prolonged temperature extremes for different Global Circulation Models.


2. Relationship between files, if important: The .csv files are necessary to run the .R scripts

3. Additional related data collected that was not included in the current data package: no 

4. Are there multiple versions of the dataset? No
	A. If yes, name of file(s) that was updated: 
		i. Why was the file updated? 
		ii. When was the file updated? 


METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: 

These data support a study that parametrised an individual-based model (IBM) using data from a population of habituated, wild meerkat groups around the Kuruman River Reserve, South Africa. During weekly visits, data on birth, death, emigration, reproductive condition, body mass, and social status were collected from individually tagged meerkats. We used the individual-level data of 1,194 females and 1,497 males from 85 groups to generate monthly, discrete-step censuses of individuals between January 15, 1997 and December 15, 2018.

Interannual deviations from the seasonal means in rainfall and temperature were calculated from long-term gauge and satellite data, obtained from NOAA’s Climate Prediction Center (CPC). To calculate monthly deviations of weather used in demographic-rate models, we first obtained total daily rainfall and the mean of the maximum daily temperature of the 1.5 months prior to each census. For both variables, we then created standardized deviations from monthly means. 

In order to assess how likely our projections using observed weather trends (i.e., sampling years with increasingly more extreme temperature deviations) are under projected climate change from global circulation models, we obtained future estimates of maximum temperatures from the Coupled Model Intercomparison Project 5 (CMIP5) used by the Intergovernmental Panel on Climate Change (IPCC). All data were freely available online at the World Climate Research Programme (https://esgf-node.llnl.gov/projects/cmip5/). The CMIP5 provides projections from a suite of global circulation models (GCMs). These GCMs produce climate projections for four atmospheric greenhouse gas Representative Concentration Pathways (RCPs): one high pathway with radiative forcing up to 8.5 Watts per square meter (Wm^2) by 2100 (RCP 8.5), two intermediate pathways (RCP 4.5 and RCP 6.0), and one low pathway (RCP 2.6). We also obtained reconstructed historical simulations (1997-2005). 


2. Methods for processing the data: 

All data were processed in R

3. Instrument- or software-specific information needed to interpret the data: 

R statistical software, version 4.0.1 (and packages as described in the R scripts)

4. Standards and calibration information, if appropriate: 

5. Environmental/experimental conditions: NA

6. Describe any quality-assurance procedures performed on the data: All R scripts are fully commented and have been checked; all raw data was quality-checked at the study site by data managers.

7. People involved with sample collection, processing, analysis and/or submission: All authors + volunteers at Kalahari Meerkat Project (https://kalahariresearchcentre.org/) 


DATA-SPECIFIC INFORMATION FOR: [age.mass.domM.csv]

1. Number of variables: 4

2. Number of cases/rows: 262

3. Variable List: 

month: numeric 1-12
year: numeric 1997-2018
AgeMonth: age in months
mass: mass as log grams


4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no


DATA-SPECIFIC INFORMATION FOR: [init_group_comp.csv]

1. Number of variables: 9

2. Number of cases/rows: 123

3. Variable List: 

ResidentGroup: group identifier
ID: meerkat individual ID
Sex: female or male 
AgeMonths: age in months
stage: life-cycle stage (P-pup; J-juveline; S-subordinate; H-helper; D-dominant)
pregCat: pregnancy state of females (np - not pregnant; 1-one-month pregnant; 2-two-month pregnant; birth-with litter of pups) 
mass: mass as log grams
group.size.sub: number of individuals > six months of age in the group
immig: whether the male is an immigrant or not 

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no


DATA-SPECIFIC INFORMATION FOR: [past.rain.temp.csv]

1. Number of variables: 4

2. Number of cases/rows: 264

3. Variable List: 

month: numeric 1-12
year: numeric 1997-2018
rainSD: standardised deviations from long-term (1997-2018) averages of total rainfall in the past 1.5 months
tempSD: standardised deviations from long-term (1997-2018) averages of mean maximum temperatures in the past 1.5 months

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no

DATA-SPECIFIC INFORMATION FOR: [gcm_output.csv] [gcm_output_2grid_point_interpol.csv]

1. Number of variables: 6

2. Number of cases/rows: 720

3. Variable List: 

hist: proportion of GCMs where historical maximum temperatures (1997-2018) are above their long-term average for at certain number of month (see below)
proj: proportion of GCMs where projected maximum temperatures (mid-century or end-century) are above their long-term average for at certain number of month (see below)
run: whether projections for midcentury (2041-2061) or end century (2079-2100) were run
rcp: Relative greenhouse gas concentration pathway corresponding to each GCM
gcm: short name of global circulation model 
cutoff: number of month with above-average maximum temperatures 

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no


DATA-SPECIFIC INFORMATION FOR: [rainfall_GPCP_1979_2020.csv]

1. Number of variables: 4

2. Number of cases/rows: 2048

3. Variable List: 

lon: longitud
lat: latitude 
date: date
rain: mean monthly rainfall (mm)

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no

DATA-SPECIFIC INFORMATION FOR: [tempNOAA_CPC_1979_2020.csv]

1. Number of variables: 4

2. Number of cases/rows: 15341

3. Variable List: 

lon: longitud
lat: latitude 
temp: daily maximum temperatures (ºC)
date: date

4. Missing data codes: NA

5. Specialized formats or other abbreviations used: no
