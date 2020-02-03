# UK road accidents

In this exercise, I have first carry out exploratory exercise to understand more about the data and also attempted 
predictive modelling to predict whether a particular case will result in accident.

Key points from the 2 exercises

## Exploratory Analysis:
1. Where are the potential locations where emergency departments should be set up for quick response?
2. What are the common traits among the "more dangerous" locations?
3. Are there certain times of the year that are more conducive to incidents?

## Predictive Framework:
<div class="alert alert-block alert-info">
From the result derived in the previous section, we obtain a quite satisfying F1 score which is 80. However, if we drill down further and look at the confusion matrix for the prediction, we realise the accuracy of prediction for severity level 1 and 2 is rather low. There are quite a number of false positive in the prediction result. 
<br>
<br>
There are a few reasons that might affect the performance for the model. Firstly, this may be due to the uneven distribution for each severity level. For severity level 1, the test cases are much smaller compared to other severity levels, this will affect the classification model to learn the pattern when predicting for severity level 1 cases. In order to improve the model, we can have more data entries for severity 1. Besides, we realise all the features in the dataset shows low correlation with the target variable, accident severity. We might need to include other features such as the characteristics about drivers that show higher correlation with the target variable to improve the model performance. 
<br>
<br>
Once a more accurate prediction model is found, this model can help police force to prioritise their assistance when multiple accidents happened at the same time. More police force can be supplied to cases that are predicted to be more severe. This will serve as a mean to lower the impact for the accidents.
