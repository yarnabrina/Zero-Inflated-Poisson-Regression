# Analysis of Fishing Data

In the dataset available from [UCLA](https://stats.idre.ucla.edu/stat/data/fish.csv), we have data on 250 groups who went to a state park, where fishing is allowed. Visitors were asked a few questions and data on the following variables were collected:
i.	persons - number of persons in that group
ii.	child - number of children in the group
iii.	camper - whether the group brought a camper to the park or not
iv.	count - number of fishes caught by a group

On the basis of the composition of a group of visitors, we want to predict the number of fishes caught by that group. Since the variable of interest is a discrete random variable taking only non-negative integer values, we try to fit a count regression model. 

We observe that there are too many zeroes in the available data. This may be because of the fact that some groups did not fish at all and those observations are recorded as zeroes, since we do not have data on whether a group fished or not. Along with these zeroes, there are also other zeroes, because some groups could not catch any fish. This leads to much higher variation in the data compared to its mean, thereby violating one of the basic assumptions of the Poisson model. So, we opt for other count models, like negative binomial model, zero-inflated models, hurdle models, etc.

Though we expected zero-inflated models would be better for this dataset, but surprisingly negative binomial model came out to be the best model by the AIC criterion.

[Report](report_in_word.docx)
[Presentation](presentation_in_powerpoint.pptx)
[Code](code_in_R.R)
[Data](fish.csv)
