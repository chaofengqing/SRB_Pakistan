This repository presents the first subnational study of Sex Ratio at Birth (SRB) trends and projections in Nepal from 1980 to 2050. We adopt a **Bayesian hierarchical time-series mixture model** to estimate and forecast SRB across provinces in Pakistan. The model leverages an extensive database compiled from all available censuses and nationally representative surveys.

- Main paper: Chao, F., KC, S. & Ombao, H. Estimation and probabilistic projection of levels and trends in the sex ratio at birth in seven provinces of Nepal from 1980 to 2050: a Bayesian modeling approach. *BMC Public Health* **22**, 358 (2022). https://doi.org/10.1186/s12889-022-12693-0
- Technical appendix of the main paper: Chao, Fengqing; K.C., Samir; Ombao, Hernando (2022). Technical Appendix for Estimation and probabilistic projection of levels and trends in the sex ratio at birth in seven provinces of Nepal from 1980 to 2050: a Bayesian modeling approach. figshare. Dataset. https://doi.org/10.6084/m9.figshare.19077155.v1

## Repository Structure

The repository contains two parts: **code** and **data**.

- Run the master R code files to get all the results. Specifically,
  - /code/main.R: run this to get the Markov chain Monte Carlo (MCMC) posterior samples of the Bayesian hierarchical model parameters; and
  - /code/main-output.R: after finish running the master code above, run this master code to get all the related output files and plots.
- Input data for the model:
  - /data/Auxdata/:data files related to survey information, country information, covariates.
  - /data/data/M57_normal_postinfo.csv: data files regarding  to the posterior information of JAGS model. 

## Research Context

This project presents a subnational estimation and projection of Nepal’s Sex Ratio at Birth (SRB) from 1980 to 2050, based on a Bayesian hierarchical time series mixture model. While previous studies have examined SRB at the national level, our approach addresses the geographic heterogeneity of sex imbalances in Nepal—a country where regional variation is both significant and policy-relevant.We aim to estimate and project SRB for the seven provinces of Nepal from 1980 to 2050 using a Bayesian modeling approach. We compiled an extensive database on provincial SRB of Nepal, consisting of the 2001, 2006, 2011, and 2016 Nepal Demographic and Health Surveys and the 2011 Census. We adopted a Bayesian hierarchical time series model to estimate and project the provincial SRB, with a focus on modeling the potential SRB imbalance.

## Methodology

The study estimates Nepal's SRBs by seven provinces from 1980 to 2016 and projects them until 2050 using a Bayesian hierarchical time series mixture model. This model incorporates uncertainties from observations and natural year-by-year fluctuations. The full model write ups are in the [published paper](https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-022-12693-0#citeas).

We used the JAGS (Just Another Gibbs Sampler) to do Bayesian inference. The relevant R code to call JAGS and to get the MCMC samples are (they are called automatically in /code/main.R):

- /code/jags_setupMCMC.R: prior settings, MCMC settings
- /code/jags_writeJAGSmodel.R: the Bayesian hierarchical model to run in JAGS
- /code/jags_getMCMC.R: get the posterior samples using MCMC algorithm via JAGS

The model convergence is checked by the code:

- /code/jags_ConvergenceCheck.R: this code is called in /code/main_output.R

