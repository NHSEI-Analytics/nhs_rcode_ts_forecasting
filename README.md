# rcode_ts_forecasting

**Standard shareable R code being used centrally in the NHS**

We have been working on using RStudio in the last 6+ months to support activity planning, and forecasting monthly activity levels for providers and commissioners of health care. After numerous requests, the time seems right that we create a central repository for all shareable code we have been working on. 

Code is set to read in from a .csv format. With data structured in 3 columns of equal length, the first column contains monthly dates formatted '01/mm/yyyy' with each month represented by the first day in each month, the second column is a dimension column of which filters can be applied and the third column is the numeric value of interest. 

**We have included some simulated data to work with the forecasting code

The ts data is split into an in-sample period, often referred to as the 'training data', and an out-of-sample period, often called 'test data'. The training data is used to estimate parameters and model selection, whilst the test data is used to evaluate the forecasting performance of those selected models so as not to select a model that overfits the training set. For example, if 48 months worth of data is taken, the last 12 months being used as test data and the first 36 being used as training data.

Models are scored and selected on the lowest out-of-sample error, with final forecasts being generated based on the entire time series using the OOS model of choice.  

Models being applied are, ARIMA, ETS and TBATS.
