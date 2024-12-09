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
  * Open RStudio.
  * Go to `File` > `New Project` > `Version Control` > `Git` > `Clone Git Repository`.  
  * Provide the necessary details to download the project from the repository.  
  * Click `Create Project`  
#### 3. Install Required Packages
  * Run the install.packages() function in the R console to install all the required packages.
     Eg: `install.packages("shiny")` 
#### 4. Run the Shiny App
  * Run the app by executing the following command in the RStudio console:
     `shiny::runApp()`
## Branching and Pull Request Guidelines
### 1. Create a Unique Feature Branch:
- Always create a new branch for your feature or bug fix.
- Use a descriptive name like `username/feature-name`
  ```
  git checkout -b yourname/feature-description
  ```
### 2. Sync Your Branch with main Regularly:
- Use the `Sync fork` button provided on Github before making any changes.

<img width="1440" alt="Screenshot 2024-12-09 at 10 03 28 AM" src="https://github.com/user-attachments/assets/5998c1eb-b2cd-43fc-9c61-0c51be9884e9">


- Alternatively, from the command line fetch latest changes and rebase your branch before creating or updating a PR:
  ```
  git fetch origin
  git rebase origin/main
  ```
### 3. Submit a Pull Request:
- Push your feature branch and open a pull request against the `main` branch:
```
git push origin yourname/feature-description
```

### 4. Resolve Comments and Conflicts:
- Address all comments and resolve conflicts before the PR is approved.

### 5. Approval Process:
- Only PRs approved by _Sanjay Mehrotra_ or _Bhargav Kowshik_ will be merged.

### 6. Avoid Direct Pushes to main:
- Direct pushes to main are prohibited. All changes must go through a PR.






## Contact
For support or further information, please contact:

Bhargav Kowshik

Email: [bhargav\@mindshiftapps.net](mailto:bhargav@mindshiftapps.net){.email}
