## Non-Differential Sensitivity Test

Welcome to the NonDifferential Sensitivity Test repository. Here, you will find

* code to run a novel test designed to detect non-differential sensitivity of an indicator
* code to run a simulation to assess power of the test
* simulation results for some specified scenarios (in the Shiny App).

## Shiny App

Our test has been applied across various scenarios, and we developed a Shiny app that visually presents the outcomes of these simulation studies for each scenario we investigated. You can access the app via the following link: 

[Non-differentility test link](https://giorgiolimoncella.shinyapps.io/NonDifferentialityTest/)

Feel free to explore the app and delve into the details of our simulation studies and their results!

## Instructions on How to Run the Non-differentiality Test

1. Download the repository by clicking the green "Code" button at the top and selecting "Download ZIP."
2. Open the to_run_non_differentiality_test.R file located in the Rscript folder.
3. Define the characteristics of the validation sample:
    - Sample size
    - Observed prevalences
    - Positive Predictive Value (PPV)
4. Set the test parameters:
    - Significance level (alpha)
    - Number of bootstrap samples
5. Source the script to run the test.
6. Check the results in the generated folder 09_Test_results/DATE_TIME/.., where you will find:
    - A plot of the empirical bootstrap distribution of the test statistic.
    - A CSV file containing the empirical distribution of the test statistic.
    - A CSV file summarizing the parameters you set.
