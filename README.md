# cost-effectiveness-analysis
**Cost-effectiveness analysis of a novel, rapid tuberculosis diagnostic tool in rural settings in the Philippines.**

**Important**

The 'rdecision' package does not allow for the integration of the Decision Tree and Markov Model aspects of the model. This means that the outputs of the Markov Model (costs and QALYs) enter the Decision Tree at the terminal nodes as model parameters with some uncertainty.

The implications of this are that the 'Markov Model.R' script must be run first. In the same environment, the 'Decision Tree.R' can then be run, which assigns a Gamma uncertainty distribution to the outputs of the Markov Model.

NB. the only dataset that is read into the modelling process is the 'Phillipines p_dying by age.xlsx' file in the 'Data' folder of the cost-effectiveness-analysis repository, which provides the average death rate for different Filipino age cohorts, used to calculate the natural death rate in the Markov model. All other model parameter data is hard-coded into R from Excel.




