# EffPPL

We present EffPPL, a shallowly embedded domain-specific probabilistic programming library in Multicore OCaml made using effect handlers. EffPPL has the capability to perform approximate Bayesian inference for models using continuous random variables. EffPPL uses the Hamiltonian Monte Carlo for performing the inference.


## Installation Instructions

1. Clone the repo
2. In multicore Ocaml ensure that the following are installed:-
  * owl 
  * owl-plplot 
  * base 
  * lwt 
  * re 
3. Run 
  `dune build effppl.a`
4. You can then run the PPL library.

## Tour of repository

The main inference algorithm can be found in lib/hmc.ml, this also includes the effect based algorithmic diffrentiation. For examples on how to use the library the lib/models can be checked. The results obtained by the models can be found in results/ folder. The code to compare the EffPPL code to Stan code can be seen in stan/ folder. Preliminary and Final Reports made for the UGRC can be found in the reports folder.

## Some Results

Here we show some results of the EffPPL library for a more detailed results page do kindly check the results folder.

### Linear Regression

We can see in figure below a plot showing sampled lines and the mean line. The black line indicates the line with the mean slope and constant. While the other faint blue lines indicate samples that PPL drew. 

<p align="center">
  <img width="460" src="https://github.com/Arnhav-Datar/EffPPL/blob/main/results/machine_learning/linreg.png">
</p>

### Binary Classification

Here to simulate a simple linear classifier we say that a correctly classified point is much more likelier than a wrongly classified point.

<p align="center">
  <img width="460" src="https://github.com/Arnhav-Datar/EffPPL/blob/main/results/machine_learning/class.png">
</p>

### Auto-Regressive Models

We use the autoregressive model described [here](https://mc-stan.org/docs/2_26/stan-users-guide/autoregressive-section.html). We generate data using alpha = 0.5 and beta = 1.03. As can be seen we are able to approximate both to a considerable extent. The plot on the left shows the predictions of alpha while those on the right show predictions for beta. 

<p align="center">
 <img width="460"  src="https://github.com/Arnhav-Datar/EffPPL/blob/main/results/time-series/autoreg_alpha.png" />
  <img width="460"" src="https://github.com/Arnhav-Datar/EffPPL/blob/main/results/time-series/autoreg_beta.png" >
</p>


