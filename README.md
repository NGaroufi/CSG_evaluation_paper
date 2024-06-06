# CSG_evaluation_paper
Dataset and code used for the research paper titled "Evaluating CSG based methodologies on an archaeological and historical sample"

## Datasets
The `CSG Data.csv` datasets contain the cross-sectional geometric properties of the respective bone assemblage. The `svm_results` and `lda_results` datasets contain the results from the sex estimation analysis performed, the `left.csv` and `right.csv` datasets correspond to the CSG variables of each anatomical side, and the `(bone_type)_sex` and `true_pairs` csv files contain the known information in regards to the skeletal sample. The `sorted`, `stats`, and `unsorted` CSV files contain the results from the sorting analysis.

## Code scripts
1. The `descriptives.R` scripts were used for the calculation of the summary statistics (mean, standard deviation, minimum, maximum) per sex group and anatomical side, as well as the summary statistics of the differences between the absolute values of each side.
2. The `accuracy_sorting.R` scripts were used for the calculation of the accuracy metrics from the sorting algorithm.
3. The `accuracy_sexing_065.R` scripts were used for the calcuation of the accutacy metrics from the sex estimation algorithm at 0.65 probability threshold.
4. The `accuracy_sexing_05.R` scripts were used for the calcuation of the accutacy metrics from the sex estimation algorithm at 0.5 probability threshold.

## Supplementary Analysis
This directory contains the results from the application of the sorting analysis to the original modern Greek dataset sample for the humerus bone assemblage, in order to calculate the TNR value of the algorithm on the original sample.
