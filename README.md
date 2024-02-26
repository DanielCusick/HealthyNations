### HealthyNations
This is a dataset and model I built in Fall 2023 as part of a paper on the potential impact of single payer health care in the United States. All data is from [stats.oecd.org](https://stats.oecd.org/). [This analysis  from the CommonWealth Fund](https://www.commonwealthfund.org/publications/2019/apr/considering-single-payer-proposals-lessons-from-abroad) informed my classification of the countries included. 

health16.csv is the final data table. At the outset of the project, I set out exploring and assembled the table piece-by-piece. Future work could consider more eleganlty coding the construction of the data table. A function for imputation of mean by country would likely make sense, for example.  

Selected variables and a snapshot of the results are summarized in the Single Payer Slides Summary.pdf file. 
The first model shows single payer systems are associated
with 90 fewer annual avoidable deaths per 100,000 relative to the U.S. system. The second model
shows single payer systems are associated with 4 additional years of life expectancy. Both
models have an R-squared measure above 0.8. The residual diagnostics are better than I expected without any transformations. It’s also worth noting the “Other” universal
government-regulated systems perform better than both the single payer and U.S. categories.
This provides a great lead for future research.
