## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(VIBER)
library(tidyverse)

## -----------------------------------------------------------------------------
data("mvbmm_example")

## -----------------------------------------------------------------------------
mvbmm_example$successes

## -----------------------------------------------------------------------------
??variational_fit

## -----------------------------------------------------------------------------
fit = variational_fit(
  mvbmm_example$successes,
  mvbmm_example$trials
)

## -----------------------------------------------------------------------------
fit = choose_clusters(fit, 
                      binomial_cutoff = 0, 
                      dimensions_cutoff = 0,
                      pi_cutoff = 0.02)

## -----------------------------------------------------------------------------
fit 

## ---- fig.height=3, fig.width=3-----------------------------------------------
plot(fit)

## ---- fig.height=3, fig.width=3-----------------------------------------------
plot_ELBO(fit)

## ---- fig.height=3, fig.width=5-----------------------------------------------
plot_peaks(fit)

## ---- fig.height=3, fig.width=3-----------------------------------------------
plot_mixing_proportions(fit)

## ---- fig.height=5, fig.width=4-----------------------------------------------
plot_latent_variables(fit)

