# String Similarity Using Transfomations

## Description

One problem that arises with nucleotide and protein sequences is finding similar sequences. Often these methods employ computationally expensive metrics such as Levenshtein distance and Smith-Waterman algorithm. It would be useful to examine other metrics that could give approximations to those metrics. Such approximation metrics only make sense when large amount or long sequences are used; otherwise we could use computationally expensive, but more exact, metrics.

The project idea is to combine transformations over the sequences to create a new metric and evaluate it against Levenshtein distance. We intend to examine such transformations as: Wavelet, Fourier, Haar and different smoothing filters. After combining we use clustering on nucleotide sequences to evaluate the differences from Levenshtein.

The main risk factor is failure to develop good enough function, because the combining different transformations doesn't work.

The vision is to create a metric that has similarity to Levenshtein and can be used to do faster similarity searches.

## Plan

### Milestones

The project can be divided into two parts:

* Theory research [Egon]
	* idea for project [done]
	* metrics theory
	* transformation functions
	* combining transformations
* Theory evaluation [Olga]
	* choosing evaluating strategy
	* choosing dataset
	* code for evaluating metrics
	* evaluating methods
		* Smoothing
		* Fourier
		* Wavelet
		* Haar
		* other ...
* Managment
	* Project Plan
	* Report
	* Presentation

### Deliverables

* Analysis of using transformation functions as a metric.
* A framework for testing approximate metrics.
* A metric for approximating Levenshtein.

