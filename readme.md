# Macrofinancial stability index #

The code in this repository compiles the macrofinancial stability index (MSI) presented in Dafermos et al. (2023) and generates the figures for the paper.

Source data are downloaded directly from various sources, cleaned and compiled by `download_data.R` and saved as `country_data.csv`. You will need an API key for FRED, add this add the top of the file.

The MSI is compiled by `mk_msi.R` using `country_data.csv` as input. The compiled MSI is written to `msi.csv`, along with some alternative versions with slightly different specifications for an online appendix.

The figures are generated from `msi.csv` and the other MSI csv files by `msi_plots.R`.

# Reference #

Dafermos, Y, Gabor, D. and Michell, J., ["Institutional Supercycles: An Evolutionary Macro-Finance Approach"](https://www.rebuildingmacroeconomics.ac.uk/managing-supercycles), Rebuilding Macroeconomics Working Paper 15, 
