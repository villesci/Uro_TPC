# Urosalpinx thermal performance curve manuscript

*Urosalpinx* thermal performance manuscript for submission to ProcB, *Environment and phenology shape local adaptation in thermal performance*

## Scripts

There are three scripts in this repository. Each has a corresponding markdown document, so that it is not necessary to run the script to see an overview of our analysis. Extensions of each accompanying markdown document are noted below. 

1) "uro_growth_rate_bin_rtpc_glmm.RMD" - This rmarkdown file contains the main statistical analysis used in this paper. This is where we fit models to thermal perofrmance data, extracted topt and MTP, and performed model selection on these variables with a set of extracted environmental parameters. markdown = "uro_growth_rate_bin_rtpc_glmm.doc"

2) "test_env.RMD" - This file is an overview of environmental sources we reviewed and selected from to calculate the environmental parameters in the above Rmarkdown file. For many sites, we show multiple data sources to support our selection of certain sources based on data quality, availability, etc. markdown = "test_env.html"

3) "TPC_bootstrap_glmm.RMD" - This file contains the code used to bootstrap our TPC curves and produce confidence intervals about our data. This is also where we produced figure 3 in our manuscript. markdown = "TPC_bootstrap_glmm.html"

## Data Files

There are three main data files in this repository:

Villeneuve et al uro growth.csv

Initial and final growth measurements and metadata about common garden experiment conditions used in this study. code = unique code for individual snails entering the experiment, pop = unabbreviated population name,  temp = temperature in degrees Celsius of the common garden experiment temperature treatment, hatch = date of juvenile snail hatching from egg case, mm/dd/yyyy format, exp.date = date of entry of juvenile snail into common garden experiment, mm/dd/yyyy format, grow.date = date of final growth measurement 24 days after exp.date, mm/dd/yyyy format, alive = snail alive at end of common garden experiment (y=yes, n=no), rem.oysters = if live feed oysters were recorded in the tea strainer half, indicating maintenance of ad libitum conditions (y=yes, n=no), cal.length.start = initial length of juvenile snail in mm before entry into common garden experiment on exp.date date as recorded via image analysis in Image J to the thousandths decimal place, cal.length.end = end length of juvenile snails in mm after removal from common garden experiment on the grow.date date, measured using digital calipers to the hundredth decimal place, cal.length = 24 day growth rate in mm, as calculated by cal.length.end – cal.lengt.start, wt = weight in grams, ran.out = if at any point in the 24 days snails ran out of feed oysters and were therefore not under ad libitum conditions (0=no, 1=yes), bin = population replicate for each temperature treatment; three separate bins in each water bath where three snails from each population were distribute in each, oce = population ocean origin, (a=Atlantic, p=Pacific).


Villeneuve et al consumption.csv

Consumption rate data of Urosalpinx on juvenile oysters recorded during the common garden experiment. Consumption rate was not analyzed as part of this paper, but is presented here as some snails ran out of food during the common garden experiment and therefore were excluded from growth analysis for not meeting ad libitum conditions. TPC Label = unique code for individual snails entering the experiment, tank.replicate = population replicate for each temperature treatment; three separate bins in each water bath where three snails from each population were distribute in each, Population = unabbreviated population site code, Temp = temperature in degrees Celsius of the common garden experiment temperature treatment, Date Hatched = date of juvenile snail hatching from egg case, mm/dd/yyyy format, Date TPC = date of entry of juvenile snail into common garden experiment, mm/dd/yyyy format, timepoint = assigned number to one of the three times we checked for consumption in tea strainers during the 24 days, date.feed = date when number of oysters consumed was checked and fresh oysters were placed in the tea strainers, date in mm/dd/yyyy, duration = length in days from previous timepoint, which is effectively the number of days where no oysters were added, rate = consumption rate as calculated by no.consumed/duration to give the number of oysters consumed per day, all.consumed = if all oysters in the tea strainer were consumed, (y=yes, n=no)

ci_extra_params_all_glmm.csv

Table of model estimates and confidence intervals around thermal performance curves calculated in the TPC_bootstrap_glmm.RMD. Not strictly needed for analysis, but the bootstrapping takes a long time and is useful to read-in to recreate our plots. param = TPC parameter being estimated,	2.50% = 2.5% confidence estimate of parameter,	97.50%, 97.5% confidence estimate of parameter,	method = type of bootstrapping used,	estimate = parameter estimate,	code = population-bin code used for each TPC.


### Environmental Data Files

We used several environmental data files to gather as complete as possible water temperature data for four years between 2012 and 2019. This data was used in both uro_growth_rate_bin_rtpc_glmm.RMD and test_env.RMD. 

Because these files originated from different sources, the data strucutre was different among many of the data files. Therefore, we have grouped the data files by site and indicate the data structure for each. Multiple years of data sometimes had to be downloaded as separate files from the source. In some cases, data files when downloaded contained superfluous data that was not used in this analysis (ie chlorophyll concentrations, depth, air temperature, etc.). In these cases, we report only the metadata actively used in this analysis. 

#### Great Bay, NH

GRBGBWQ.csv 
Sourced from NERRS. Station_Code = station identifier,	Temp = Water tempreature in degrees Celsius, DateTimeStamp = local date time stamp of the data observation in mm/dd/yyyy format.


#### Woods Hole, MA

bzbm3h2014.txt, bzbm3h2014.txt, bzbm3h2016.txt, bzbm3h2017.txt, bzbm3h2018.txt, bzbm3h2019.txt
Sourced from NOAA NDBC. YY = four digit year, MM = month, DD = day, hh = hours under 24-hour clock, mm = minute, WTMP = water temperature in degrees Celsius.  

#### Oyster, VA

oy.csv
Sourced from Virginia Coast Reserve LTER. STATION = station identifier, DATE = date stamp in mm/dd/yyyy format, TIME = time under a 24 hour clock, minute and hours not separated by a character, WTEMP = water temperature in degrees Celsius. 

#### Beaufort, NC

bftn7h2014.txt, bftn7h2015.txt, bftn7h2016.txt, bftn7h2017.txt,bftn7h2018.txt, bftn7h2019.txt, bftn7h2020.txt
Sourced from NOAA NDBC. YY = four digit year, MM = month, DD = day, hh = hours under 24-hour clock, mm = minute, WTMP = water temperature in degrees Celsius. 

#### Folly Beach, SC

chts1h2014.txt, chts1h2015.txt, chts1h2016.txt, chts1h2017.txt,chts1h2018.txt, chts1h2019.txt, chts1h2020.txt
Sourced from NOAA NDBC. YY = four digit year, MM = month, DD = day, hh = hours under 24-hour clock, mm = minute, WTMP = water temperature in degrees Celsius. 

#### Skidaway, GA

gcsk.csv
Sourced from Skidaway Institute of Oceanography. STATION = identity code for the station, of which there were two: S2 and S8. We used S2 based on completness of record, date = date stamp in mm/dd/yyyy format, time = local time on 24 hour clock, WTMP = water temperature in degrees Celsius, site = site code applied by the authors (grove creek skidaway).

fpkg1h2014.txt, fpkg1h2015.txt, fpkg1h2016.txt, fpkg1h2017.txt, fpkg1h2018.txt, fpkg1h2019.txt, fpkg1h2020.txt
Sourced from NOAA NDBC. YY = four digit year, MM = month, DD = day, hh = hours under 24-hour clock, mm = minute, WTMP = water temperature in degrees Celsius. 

saphdwq2013.csv, saphdwq2014.csv, saphdwq2015.csv, saphdwq2016.csv, saphdwq2017.csv, saphdwq2018.csv, saphdwq2019.csv, saphdwq2020.csv
Sourced from NERRS. StationCode = station identifier, DateTimeStamp = local date time stamp of the data observation in mm/dd/yyyy format, Temp = water temperature in degrees Celsius. 

SAPDCWQ.csv
Sourced from NERRS. Station_Code = station identifier,	Temp = Water tempreature in degrees Celsius, DateTimeStamp = local date time stamp of the data observation in mm/dd/yyyy format.

SAPCAWQ.csv
Sourced from NERRS. Station_Code = station identifier,	Temp = Water tempreature in degrees Celsius, DateTimeStamp = local date time stamp of the data observation in mm/dd/yyyy format.

#### Willapa, WA

wp.csv
Sourced from Pacific Shellfish Institute (PSI). DateTimeStamp = local date time stamp of the data observation in mm/dd/yyyy format, WTMP = water temperature in degrees Celsius

tokw1h2013.txt, tokw1h2014.txt, tokw1h2015.txt, tokw1h2016.txt, tokw1h2017.txt, tokw1h2018.txt, tokw1h2019.txt, tokw1h2020.txt
Sourced from NOAA NDBC. YY = four digit year, MM = month, DD = day, hh = hours under 24-hour clock, mm = minute, WTMP = water temperature in degrees Celsius. 

#### Humboldt, CA

ind.csv
Sourced from CeNCOOS. date = date in mm/dd/yyyy format, hour = 
timestamp under a 24 hor clock, WTMP = water temperature in degrees Celsius, site = site code applied by the authors.

wiyot2011.csv, wiyot2012.csv, wiyot2013.csv, wiyot2014.csv, wiyot2015.csv, wiyot2016.csv
Sourced from the Wiyot Tribe. Date & Time = date time stamp in mm/dd/yyyy hh:mm format, Temp C = water temperature in degrees Celsius.

hm.csv
Sourced from Humboldt University. date = date stamp in mm/dd/yyyy format, time = local time on 24 hour clock, WTMP = water temperature in degrees Celsius, site = site code applied by the authors.

 