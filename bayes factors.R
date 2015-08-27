#allikas: http://bayesfactor.blogspot.com/2014/02/bayes-factor-t-tests-part-1.html
#Carole does not believe that LH increases sleep at all, while Paul claims 
#that it does.
improvements = with(sleep, extra[group == 2])
improvements

#We begin our analysis by computing N and the t statistic from the data:
N = length(improvements)
t = mean(improvements)/sd(improvements) * sqrt(N)
t

#Typically, at this point we would look up the p value for this t statistic; 
#but for a Bayes factor analysis, we use it to compute the relative evidence 
#for the two hypotheses. Of interest is the effect size:
deltaHat = t/sqrt(N)
deltaHat
#The effect size δ^=1.1637 seems to favor Paul, since Paul's hypothesis (δ=1) 
#was actually quite close to the observed result .

#Under Carole's hypothesis, the t statistic has a central t distribution with 9
#degrees of freedom; Under Paul's hypothesis, the t statistic has a noncentral 
#t distribution with 9 degrees of freedom, and a noncentrality parameter of 
#δN−−√=1×10−−√. The ratio of the two densities, at the observed t statistic, 
#yields the Bayes factor.
dt(t, df = 9, ncp = 1 * sqrt(10))/dt(t, df = 9)

dt(t, df = 9)/dt(t, df = 9)
#sama asja siis ,kui võrdlus ei ole nii kindel (Paul arvab, et umbes kuskil
#seal võiks vastus olla)
library(BayesFactor)
#The nullInterval argument tells ttestBF that you want to consider that range 
#as a hypothesis. Paul's hypothesis ranged from 0 to ∞, so we specify that 
#range using the nullInterval argument:
ttestBF(improvements, nullInterval = c(0, Inf))

