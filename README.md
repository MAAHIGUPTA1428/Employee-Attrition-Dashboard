#Employee Attrition Dashboard
This project is a Shiny dashboard designed to help analyze employee attrition patterns using interactive visualizations.
Users can explore factors such as age, salary, job role, education, experience, work-life balance, and other HR metrics to understand trends behind employee turnover.

##Project Structure
Employee-Attrition-Dashboard/
│── attrition_dashboard.R
│── Employee Attrition.csv
│── README.md

##Features
-Interactive filters for exploring attrition trends
-Visualizations for demographics, job roles, income, and satisfaction levels
-Clear insights to support HR decision-making

##Requirements
Install these R packages before running the app:
install.packages("shiny")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("shinydashboard")

##How to Run
1.Open RStudio
2.Set working directory to your project folder
3.setwd("path_to_your_folder")

##Run the app
shiny::runApp()

##Dataset
The dashboard uses the dataset:
**Employee Attrition.csv**
