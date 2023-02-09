MagicGlasses2

## Installation

Open a new project in RStudio and select 'Project from Version Control'. Select git repository jpainter/MagicGlasses2

## Configuration 

- Got to the RStudio console tab, Files, and open the file MagicGlasses2.Rmd (click on the link).

RStudio may suggest installing some needed packages such as Markdown and Shiny.  Please say yes to ensure that you are working with the latest version of packages. 

- To install other required libraries, either run (type command below)

renv::restore()

or open and run the file installPackages.R


## Workflow

- To start the app, press on the Run Document button on the MagicGlasses2.Rmd tab. 

- The app will open to the Setup and Login page. 

- The overall design is to progress from left to right across the headings, from *Setup and Login*, to *Metadata*, *Data*, *Reporting*, *Outliers*, and *Evaluation*.   This parallels the recommend path for analyzing HMIS data: first define the data required, request the data, review the data for reporting bias and outliers, and lastly, evaluate trends and/or intervention effectiveness.  

## Setup and Logon 

- On the left side, enter the Directory for data files. This is the folder where you will store downloaded metadata and data.  The file must exist before it can be used.  When the MagicGlasses2 finds the folder, it will show a list of metadata files in that folder, which is just to confirm that you have entered the correct folder address.  
 
- On the right side, you may enter credentials and url for a specific DHIS2 instance.  These details may be entered manually, or saved to a file that stores your credentials.  By default, the app looks in the file Instances.xlsx (within the MagicGlasses2 project), which contains links to the DHIS2 demo instances.  

- It is only necessary to logon when actively downloading metadata or data. Otherwise, the app will display previously downloaded values and you may continue to work offline.

- When logged on, the app will display note this and display some details about the instance (e.g. version).  

- If you wish, you may copy this file as _Instances.xlsx and add your personal credentials to other instances.  If the app finds a file named _Instances.xlsx, it will open that instead of Instances.xlsx.  

- [NOTE: Please do not delete Instances.xlsx as this may cause an issue when pulling a newer version of MagicGlasses2 from the GitHub repository. (See the section Updating MagicGlasses2 for more details)]

## Metadata 

- Request metadata by pressing button on the left side (you must be logged on)

- The screen will display the requests (dataElement, Categories, orgUnits, etc) as they are being requested.

- When finished **Save metadata ** to the data folder by using the button on the rights side.  

- [NOTE:  The metadata is not automatically saved.  The app creates a spreadsheet with separate sheets for each piece of metadata (e.g. orgUnits, dataELements) that can be reviewed outside of the app.]

- The next time that you open the app and provide a folder directory, the most recent metadata will be loaded and it is not necessary to re-request the metadata.  It is good practice, though, to periodically check for updated metadata. 

- The Metadata page has several tabs to browse.  **To find specific items** there are two ways to search.  One is the global search box--it looks for the search word in all columns.  The second way is to enter a search word in the box at the top of the column; that search is only in that column.  It often helps to use both strategies.  For example, to find the data elements for confirmed malaria cases, start by entering 'malaria' in the global search box.  Then, in the dataElement column, try search words like 'confirmed' or 'positive' or '+'.

## Data

This page is used to define the data for analysis (dataElements, categoryOptionCombos, and then to request the data

- ### Data dictionary (formulas)

- Create and update formulas (data dictionary).  Before downloading any data, you must define which elements and categories will be included.  This data dictionary (also called formulas) will be stored in an excel file with the name 'formulas_*date*'.  

 -- The data page is divided into two columns.  
 
 -- On the left side, select an existing formula file ("formula.file").  If none, just continue.  
 
 -- Start by typing name of formula in the select box on left side ("Select Formula:"). If a starting with a file of formulas, this name will be added to the file (once it is saved--see below--not right away).  Be sure to hit enter after typing the name.
 
 -- On the right side, there are two tabs, ‘formula elements’ and ‘data elements’. 
 
 -- On the 'data elements' page, select elements from the data elements tab; they will turn blue and  get added to the formula elements.  Re-clicking on data element deselects it. Use the search fields at the top of each column to find the desired elements.  For example, when searching for monthly confirmed cases, you may want to put 'monthly' in the period column; 'confirmed' in the data element column; and 'malaria' in the data element group column.

  -- Switch to the 'formula elements' tab to see results.  Note that for each data element there is a row for each category-combination (e.g. age disaggregations) 
  
  -- Save once formula selected -- there is a save button on top-middle of screen.  Save the file in the same folder you used on the setup page. Unfortunately the new formula will not show up in the app until you close and restart the app.( Still trying to figure that out…). Once you have a formula file you can add more formulas to it.  
  
- ### Request data

- To update data, click on the file to load it, then request data.  When a data file is already loaded, the app will check the previous national level values in file against the ones on the server.  If the values are the same for a given period (e.g. Month), then the app skips to the next period. This prevents re-requesting data that is unchanged and may save a lot of time.  

- If the national level values are different, all of the orgUnit values will be downloaded for that period. The app does not try to figure out which orgUnit had an updated value.  However, a data file with the 'old' and 'updated' data is saved to the data folder.  The file name will begin with 'update_compare'.

## Reporting

## Outliers

Several outlier algorithms are employed in succession, beginning with the most lax, *mad15*, and continuing to the most restrictive, *seasonal3*. The algorithms are applied to values entered at each orgUnit for each dataElement_category.  


The algorithms are, in order, 

- ### key_entry_error is a search for potential mobile phone entry codes that end up getting entered as the data.  The algorithm looks for values with at least 4 digits that occur more often than would be expected by random.  

- ### over_max is removes any values greater than a provided max value.  For monthly stock out data, we set a max of 31 (days). (Typically, no max is set for any other data element)  

- ### Extreme Values (MAD) 

Rather than using an algorithm base on values outside a mean +/- SD, we use the the mean absolute deviation (MAD) [https://en.wikipedia.org/wiki/Median_absolute_deviation]. The idea is similar to a mean and standard deviation approach, but is more robust to outliers.  When there are multiple outliers in a series, the values may not be detected by the mean +/- standard deviation (or x times the SD), because the outliers may greatly inflate the mean and sd.  Therefore, the MAD is recommended [https://www.sciencedirect.com/science/article/abs/pii/S0022103113000668]. 

-- ### mad15 is similar to looking for values outside a mean +/- SD, but uses the median instead of the mean, and the median absolute deviation (mad), instead of the standard deviation.  The magnitude of deviation is set at 15x the mad.  


The code is:
deviation = 15 # for mad15 (10 for mad10)

medianVal = median( y , na.rm = TRUE )

medianAbsDeviation = mad( y , na.rm = TRUE )

mad = y > ( medianVal + deviation * medianAbsDeviation ) |
          y < ( medianVal - deviation * medianAbsDeviation )
          
The values are flagged as a potential error when mad is TRUE.  The dataset is updated with a column named mad15 (and later, mad10) and the values of mad (from code above) are inserted for each row of orgUnit, Month, data element, and category combination.  


-- ### mad10 begins with data after removing values > mad15 , and then applies same code as mad15 but uses a deviation of 10 instead of 15.  The step-wise process is done because the occurrence of several very extreme outliers (mad15) can prevent detection of other extreme values.

### Seasonal Outliers 

After removing extreme values within each series, we use the date period (month or week), to look for values that may not be outside the overall range of values, but that appear unlikely given other values during the same month (or week).  

Two algorithms are run to detect seasonally adjusted outliers, which are outliers when compared with values during the same time period, such as high value when value are usually low, or a low value when values usually run high. These values are not necessarily the largest or the smallest values, but their timing is incongruous with other values during the same time of the year.  


For each value in the dataset, we calculate an expected value using the tsclean function (forecast package).  The *seasonal5* algorithm looks for values 5x greater than the difference between the observed and expected, divided by the MAD.  Values that pass the *seasonal5* algorithm are subsequently tested with the *seasonal3* algorithm, which reduces the allowable range of values to 3x abs( observed - expected )/MAD


The code is:

    x.ts = x %>% as.ts( . , frequency = 12 ) 
    x.forecast  = tsclean( x.ts , replace.missing = interpolate ,lambda = .lambda ) %>%
                as.integer()
    MAD = mad( x , na.rm = TRUE )
    outlier = abs( ( x.forecast - x.ts ) / MAD ) >= deviation


[coming soon: An alternative set of outliers flages from the tsoutlier function in the forecast package, which compares values to an STL model (https://robjhyndman.com/hyndsight/tsoutliers/)]


## Evaluation

The evaluation section follows a process outlined by Linden in [*Evaluating-Disease-Management-Program-Effectiveness-An-Introduction-to-Time-Series-Analysis*] (https://www.researchgate.net/profile/John-Adams-28/publication/8909526_Evaluating_Disease_Management_Program_Effectiveness_An_Introduction_to_Time-Series_Analysis/links/0046352152020e4f60000000/Evaluating-Disease-Management-Program-Effectiveness-An-Introduction-to-Time-Series-Analysis.pdf).

The time-series models are estimated with using the tsibble and feasts packages.  The predicted values are then estimated with the fable package, as described in this [blog](https://robjhyndman.com/hyndsight/fable/) 

- **Background on modeling with grouped and/or hierarchical time-series models**. The data in this project is typically grouped by orgUnit hierarchy as well as categories like age and sex.  While the display shows the total across all cateogries, the modeling may be done at each category level and then aggregated, a process called reconciliation.  The principles and R code are described in the book [**Forecasting: principles and practice**](https://otexts.com/fpp3/hierarchical.html) by the authors of the tsibble and fable packages. The aggregation levels are specified with the aggregate_key function.  [This post](https://notast.netlify.app/post/2021-06-09-hierarchical-forecasting-of-hospital-admissions-classical-forecast/) provides a worked example and helps illustrate the method.  


## Other Notes

- Screensize: Like many web apps, MG2 tries to fit content into one screen, but does not always succeed.  To better view the contents, try zoom ( press CTRL -Shift and  + keys) and un-zoom (press CTRL and - keys).


