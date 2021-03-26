## Load prediction tools for JEP paper
# Jann Spiess, March/April 2017

# This script replaces loading of an R package for easier local replication


# Load prediction algorithms
source("predictiontools/algo_boosting.R")  # Boosted tree
source("predictiontools/algo_linear.R")  # OLS et al.
source("predictiontools/algo_penalizedlinear.R")  # Elastic net (incl. LASSO)
source("predictiontools/algo_randomforest.R")  # Random forest
source("predictiontools/algo_tree.R")  # Regression tree

# Load tools
source("predictiontools/tool_analysis.R")  # Prediction output analysis
source("predictiontools/tool_dataprep.R")  # Data preparation
source("predictiontools/tool_prediction.R")  # Main prediction analysis routine
source("predictiontools/tool_predictionhelpers.R")  # Tuning and ensemble tools

# Load interface
source("predictiontools/interface_predictiveanalysis.R")  # Main interface for local analysis of ensembles
source("predictiontools/interface_serverparallel.R")  # Additional tools for manual parallelization


