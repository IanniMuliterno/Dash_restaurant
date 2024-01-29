# Shiny + AI: Restaurant Customer Satisfaction Dashboard

This repository contains a dashboard project that aims to analyze customer satisfaction survey responses. The dashboard is powered by artificial intelligence, which provides more accurate insights for action. The way I interact with the AI is a trade secret, that's why I bring it as a static result in this repository, but to understand how to connect with OPENAI API via R, check `api_gpt_playground.Rmd`.

### See it live [here](https://ianmuliterno.shinyapps.io/Customer_Sat_2/)

Ps: [Appsillon](https://explore.appsilon.com/shiny-demo-gallery) inspired me to make it prettier, so check them out!


## Dependencies

The libraries used in the project are:

* shiny
* shinydashboard
* ggplot2
* tidyverse
* hrbrthemes
* stringr
* tidytext
* stopwords
* tm
* syuzhet
* httr
* jsonlite

## How to run the project

To run the project, follow these steps:

1. Install the required libraries.
2. Clone the repository.
3. Open the `app.R` file in RStudio.
4. Run the code in RStudio.

The dashboard will open in your browser.

## Features

The dashboard offers the following features:

* Visualization of general survey data, such as the number of respondents, the average rating, the main positive and negative points, etc.
* * Actionable insights to increase customer satisfaction, powered by AI.
* in the Analytics tab, we have analysis of customer sentiment, using artificial intelligence techniques.


## Next steps

The next steps for the project include:

* add a comparison with previous period. eg. 10% more replies than last period (be it month, weekday, etc).
* Allow user to interact with AI for further support and insights on what to do.
* Analysis of satisfaction by customer, grouping by demographic data, such as age, gender, etc.

## Contributions

Contributions to the project are welcome. To contribute, follow these steps:

1. Fork the repository.
2. Make the desired changes.
3. Create a pull request.

## Contact

For questions or suggestions, please open an issue or send an email to ianmuliterno@gmail.com.
