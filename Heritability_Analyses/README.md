
### Heritability Analyses


This README file contains a description of the files and scripts used to estimate the heritability of age at maturity over a 10 year spawning period in a Delta smelt hatchery.

## Pedigree Files

Files in this folder contain the pedigree information for each fish born in the hatchery subset by birth year. The first column in the file denotes the animal ID, the second column is the sire (father) ID, and the third column is the dam (mother) ID. These files are required input for the maturation_glmmMCMC.R script.

## Running the model

maturation_glmmMCMC.R is the main script file for estimating the heritability of age to maturity for each spawning year. Because these analyses take exponentially longer the larger the pedigree files (such as 2020 and 2021), the R script can be submitted on an HPC using the maturation_heritability.sh bash script. For the year 2010, this took about ~2hours to run, whereas the year 2021 took almost 7 days to run. The input files include a pedigree file (see Pedigree folder) and the AgeAtMaturity_data.txt file. The model is run twice in parallel to compare the efficacy of the model parameters.

## Calculating Phenotypic variation, genotypic variation, and heritability

Once the model has completed its run, import the output from maturation_glmmMCMC.R into maturation_heritability_results.R (this script can be run on a local desktop). We have provided an example output file from maturation_glmmMCMC.r: DS_hert_models_maturation_parallel_2010_2runs_fixedtemp.RData which can be opened in maturation_heritability_results.R
This script will compare the model outpruts from the two runs to make sure they converge and the correct model parameters have been chosen. The script will then also calculate phenotypic variation, genotypic variation, and heritability from the model results. Finally, the script provides code to graph the results for each year using the input file: heritability_Vp_results_combined_nitt500thousand_fixedtemp.txt


