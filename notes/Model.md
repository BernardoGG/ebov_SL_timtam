# Sierra Leone EBOV analysis - Model specification
_Bernardo Gutierrez_

Model parameters specified in BEAST 2 XMLs

1. We use a strict clock; we set a prior as a normal distribution with median rate of 0.0012 s/s/y and bound the values across all estimates from the Holmes _et al_ analysis: the lower bound corresponds to Hoenen _et al_ lower bound (0.00089 s/s/y) and the upper bound is from Gire _et al_ (0.00291 s/s/y). This results in a rate of 0.00000329 s/s/d (bound as 0.00000244 s/s/d to 0.00000797 s/s/d).
2. We use an **HKY + Γ** substitution model (4 discrete categories to model the Γ distribution for rate heterogeneity).
3. From Gire _et al_ we set a rough estimated origin for the Sierra Leone epidemic at 23 April 2014, approximately around the time where the split of both SL1 and SL2 lineages took place. This is **102 days** before present (the last date in the data set).
4. On a 2015 systematic review, Velasquez _et al_ report an incubation (latency) period of 6.22 ± 1.57 days from exposure to the start of the infectious period, and an infectiousness period of 9.40 ± 5.50 days - this latter estimate corresponds to survivors (infectious period for patients who do not survive is estimated as 5.33 ± 4.03; we use the longer time period from both).
5. We use a reporting rate of 1 in 3 cases (0.33) following estimates by Gignoux _et al_ (2015) and Dalziel _et al_ (2018). The weekly sequencing rate from the data ranges between 2.5% and 70.6% with a median of 27.9%.
6. We add a change in the R0 estimate 76 days before present, when te first sequence was collected.
