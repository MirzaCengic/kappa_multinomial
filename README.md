
# This is a brief tutorial for using `kappa_multinomial`, a new way to assess the performance of multinomial predictions for landcover 

Increaingly, land cover models use multinomial models to predict occurence of discrete classes. At present, no suitable mesure exist to evaluate the agreement of such models with observations.

This package provides a method $k_multinomial$ to assess the agreement between model with probabilistic output (i.e. predicting an occurence probabity for classes of outcomes) and observations. $k_multinomial$ may be used to compare model agreement to observations with discrete outcomes (one succes for exactly one out of q classes) and continuous outcomes (each class is observed with a frequency).

We refer to Run_Kappa for examples and application of the above mentioned.


