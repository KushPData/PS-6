# User document for PS-6

## About page:
The data used in this problem set includes the responses of 253,680 individuals who took the Behavioral Risk Factor Surveillance System (BRFSS) survey in 2015. 

The data was taken from the Kaggle Diabetes Health Indicator Dataset. 

## Plot Page:
radioButtons: Widget that allows user to change the background color of the plot. For aesthetic purpose only. 

checkboxGroupInput: Widget that allows user to select the underlying health conditions they wish to analyze. 

The plot: The plot on the right-hand side of the page displays the number of patients with the underlying conditions selected by the user. It also divides those patients into the two stages of diabetes, prediabetes and diabetes. 

textOutput: Displays the number of conditions selected by the user. 

## Table Page:
radioButtons: The first set of radioButtons allow the user to change the color of the tables. For aesthetic purpose only. 

radioButtons: The second set of radioButtons allow the users select between the two stages of diabetes, prediabetes and diabetes. 

outputTable: The first table displays the number of patients with various underlying health conditions based on the stage of diabetes selected by the user. 

outputTable: The second table displays the percentage of patients with various underlying health conditions based on the stage of diabetes selected by the user. 

textOutput: Displays the number of patients in the stage of diabetes selected by the user. 

## Shiny App Link
https://kushpdata.shinyapps.io/Diabetes/
