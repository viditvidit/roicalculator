# ROICalculator

## Brief Overview

This is a shiny app for calculating the return on investment (ROI) for large mining operations on digitising fuel accounting, dispatch and auditing. 
The shiny app has options to set fleet size and fuel volumes, and also set base salaries of people involved in the process. 
It will then calculate the man hours saved and its equivalent cost savings, as well as the ROI on digitising the process.

## R Shiny Application Setup Guide

#### 1. Prerequisites

##### Install R and RStudio
  * R is a programming language known for its extensive libraries and packages, making it a popular tool for statistical analysis and data visualization. Download URL: [CRAN R Project](https://cran.r-project.org)
  
  * RStudio is software application that makes R easier to use. Download URL:[Posit Open Source](https://posit.co/products/open-source/rstudio)

##### Install Git
  * Download URL: [Git](https://git-scm.com/downloads)
  
#### 2. Set Up the Project on your local machine
  a. Open RStudio.
  b. Go to `File` > `New Project` > `Version Control` > `Git` > `Clone Git Repository`.
  c. Provide the necessary details to download the project from the repository.
  d. Click `Create Project`
  
#### 3. Install Required Packages
  Run the install.packages() function in the R console to install all the required packages.
    Eg: `install.packages("shiny")` 

#### 4. Run the Shiny App
  Run the app by executing the following command in the RStudio console:
    `shiny::runApp()`


## Contact

For support or further information, please contact:

Bhargav Kowshik

Email: [bhargav\@mindshiftapps.net](mailto:bhargav@mindshiftapps.net){.email}