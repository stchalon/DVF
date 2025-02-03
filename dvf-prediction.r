################################################################################
# DVF (Données de Valeur Foncières) : https://explore.data.gouv.fr/fr/immobilier
#
setmlr=1
setdebug=1
setpca=1
setdl=1
setgbm=1
setsvr=1

# setsource
# 0: dvf-chaville training<2024, prediction=2024 done	>> setpca=0 (same code_postal => no eigen value)
# 1: dvf-viroflay training<2024, prediction=2024 done	>> setpca=0 (same code_postal => no eigen value)
# 2: dvf-chaville+2 samples training<=2024, prediction=2025 new	>> setpca=0 (same code_postal => no eigen value)
# 3: dvf-viroflay+1 samples training<=2024, prediction=2025 new	>> setpca=0 (same code_postal => no eigen value)
# 4: dvf-versailles
# 5: dvf-versailles prediction
setsource=5

source("C:\\Users\\steph\\RML\\dvf-loaddataset.r")

source("C:\\Users\\steph\\RML\\dvf-mlr.r")
source("C:\\Users\\steph\\RML\\dvf-pca.r")
source("C:\\Users\\steph\\RML\\dvf-dl.r")
source("C:\\Users\\steph\\RML\\dvf-dloptim.r")
source("C:\\Users\\steph\\RML\\dvf-lightgbm.r")
source("C:\\Users\\steph\\RML\\dvf-svr.r")
source("C:\\Users\\steph\\RML\\dvf-debug.r")

# plot histogram and gaussian
# plot_gaussian(dt$valeur_fonciere,training_mlr)
# https://stackoverflow.com/questions/76845577/ggplot2-legend-for-multiple-line-plots
# https://forum.posit.co/t/adding-manual-legend-to-ggplot2/41651
# DeepSeek for legend
#
plot_gaussian <- function(actual, mlr, dlo, gbmo, svr) {
  library(ggplot2)
  library(reshape2)
  library(tidyr)

  bw <- 10000
  n_obs <- sum(!is.na(actual))

  # Create a data frame for the actual data
  df1 <- data.frame(actual, mlr, dlo, gbmo, svr)

  # Melt the data frame for ggplot
  df1_long <- df1 %>%
    gather(key = "Type", value = "Value")

  # Define colors and labels for the legend
  colors <- c("actual" = "black", "mlr" = "dark green", "dlo" = "blue", "gbmo" = "red", "svr" = "purple")
  labels <- c("Actual", "MLR", "DLO", "GBMO", "SVR")

  # Plot
  ggplot(df1_long, aes(x = Value, color = Type)) +
    stat_function(fun = function(x) dnorm(x, mean = mean(actual), sd = sd(actual)) * bw * n_obs, aes(color = "actual")) +
    stat_function(fun = function(x) dnorm(x, mean = mean(mlr), sd = sd(mlr)) * bw * n_obs, aes(color = "mlr")) +
    stat_function(fun = function(x) dnorm(x, mean = mean(dlo), sd = sd(dlo)) * bw * n_obs, aes(color = "dlo")) +
    stat_function(fun = function(x) dnorm(x, mean = mean(gbmo), sd = sd(gbmo)) * bw * n_obs, aes(color = "gbmo")) +
    stat_function(fun = function(x) dnorm(x, mean = mean(svr), sd = sd(svr)) * bw * n_obs, aes(color = "svr")) +
    scale_x_continuous("x") +
    scale_color_manual(values = colors, labels = labels) +
    labs(color = "Legend") +
    theme_minimal()
}

# Example usage
if (setmlr == 1 & setsvr == 1 & setdl == 1) {
  plot_gaussian(dt$valeur_fonciere, training_mlr, training_dlo, training_gbm_optim, training_svr)
}
