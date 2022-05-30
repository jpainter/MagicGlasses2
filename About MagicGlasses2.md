
#Installation

Open a new project in RStudio and select 'Project from Version Control'. Select git repository jpainter/MagicGlasses2

#Configuration 

- Got to the RStudio console tab, Files, and open the file MagicGlasses2.Rmd (click on the link).

RStudio may suggest installing some needed packages such as Markdown and Shiny.  Please say yes to ensure that you are working with the latest version of packages. 

- To install other required libraries, either run (type command below)

renv::restore()

or open and run the file installPackages.R


#Workflow

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

- Create and update formulas (data dictionary).  Before downloading any data, you must define which elements and categories will be included.  This data dictionary (formerly called formulas) will be stored in an excel

# Reporting

#Outliers

https://en.wikipedia.org/wiki/Median_absolute_deviation 
https://www.sciencedirect.com/science/article/abs/pii/S0022103113000668


# Evaluation
