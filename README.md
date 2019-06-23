# SharedS
Functions to assist community ecologists to assess the influence of occasional species on (dis)similarity metrics.

SharedS was written in 2011, when I was an undergrad research assistant at the Insect Ecology Lab (Federal University of Lavras, Brazil) under the supervision of Dr. Julio Louzada and Dr. Jos Barlow.
It aimed to automate and expand on the procedures used in *Barlow et al (2010)-Measuring the conservation value of tropical primary forests: the effect of occasional species on estimates of biodiversity uniqueness*.

The code is released under GPL-3 licence, which means it is free to use, modify and redistribute, but any derived code needs to be under the same licence (or a later version).

# Installation

You can install SharedS directly from this repository with devtools.

```
#Install devtools if you don't already have it
install.packages("devtools")
#Load it
library(devtools)

#Use the install_github function to install from this repository
devtools::install_github("fsfrazao/SharedS")

#Load SharedS
library(SharedS)
```


# References:

* Barlow, J., Gardner, T. A., Louzada, J., & Peres, C. A. (2010). Measuring the conservation value of tropical primary forests: the effect of occasional species on estimates of biodiversity uniqueness. PloS one, 5(3), e9609. doi:10.1371/journal.pone.0009609
