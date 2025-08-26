This repository presents the first subnational study of Sex Ratio at Birth (SRB) tren ds and projections in Nepal from 1980 to 2050. We adopt a **Bayesian hierarchical time-series mixture model** to estimate and forecast SRB across seven provinces in Pakistan. The model leverages an extensive database compiled from all available censuses and nationally representative surveys.

- Main paper: Chao, Fengqing & Wazir, Muhammad & Ombao, Hernando. (2022). Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan from 1980 to 2020 with scenario-based probabilistic projections of missing female birth to 2050: A Bayesian modeling approach. International Journal of Population Studies. 8. 51-70. 10.36922/ijps.v8i2.332.

- Technical appendix of the main paper: Chao, Fengqing; Wazir, Muhammad Asif; Ombao, Hernando (2021). Web appendix for levels and trends in sex ratio at birth in provinces of Pakistan from 1980 to 2020 with scenario-based missing female birth projections to 2050: a Bayesian modeling approach. figshare. Dataset.
https://doi.org/10.6084/m9.figshare.16917622.v1

## Repository Structure

The repository contains two parts: **code** and **data**.

- Run the master R code files to get all the results. Specifically,
  - /code/main.R: run this to get the Markov chain Monte Carlo (MCMC) posterior samples of the Bayesian hierarchical model parameters; and
  - /code/main-output.R: after finish running the master code above, run this master code to get all the related output files and plots.
- Input data for the model:
  - /data/Auxdata/:data files related to survey information, country information, covariates.
  - /data/data/M57_normal_postinfo.csv: data files regarding  to the posterior information of JAGS model. 

## Research Context

This project presents a subnational estimation and projection of Pakistan’s Sex Ratio at Birth (SRB) from 1980 to 2050, based on a Bayesian hierarchical time series mixture model. While previous studies have examined SRB at the national level, our approach addresses the geographic heterogeneity of sex imbalances in Pakistan — a country where regional variation is both significant and policy-relevant. We aim to estimate and project SRB for the seven provinces of Pakistan from 1980 to 2050 using a Bayesian modeling approach. We compiled an extensive database on provincial SRB of Pakistan, consisting of the Census, Demographic and Health Surveys (DHS), Multiple Indicator Cluster Surveys (MICS), and Pakistan Social and Living Standards Measurement Surveys (PSLM). We adopted a Bayesian hierarchical time series model to estimate and project the provincial SRB, with a focus on modeling the potential SRB imbalance.

## Methodology

The study estimates Nepal's SRBs by seven provinces from 1980 to 2016 and projects them until 2050 using a Bayesian hierarchical time series mixture model. This model incorporates uncertainties from observations and natural year-by-year fluctuations. The full model write ups are in the [published paper](https://www.researchgate.net/publication/366272892_Levels_and_trends_estimate_of_sex_ratio_at_birth_for_seven_provinces_of_Pakistan_from_1980_to_2020_with_scenario-based_probabilistic_projections_of_missing_female_birth_to_2050_A_Bayesian_modeling_app).

We used the JAGS (Just Another Gibbs Sampler) to do Bayesian inference. The relevant R code to call JAGS and to get the MCMC samples are (they are called automatically in /code/main.R):

- /code/jags_setupMCMC.R: prior settings, MCMC settings
- /code/jags_writeJAGSmodel.R: the Bayesian hierarchical model to run in JAGS
- /code/jags_getMCMC.R: get the posterior samples using MCMC algorithm via JAGS

The model convergence is checked by the code:

- /code/jags_ConvergenceCheck.R: this code is called in /code/main_output.R

