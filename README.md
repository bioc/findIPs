# findIPs
The package aims at detecting IPs based on case deletion and quantifies their effects by measuring the weighted rank changes. The package applies a novel rank-comparing measure using the adaptive weights that stress the top-ranked important features and adjust the weights to ranking properties.

For full details, please see our preprint: 

Wang, Shuo, and Junyan Lu. "Detect influential points of feature rankings." arXiv preprint arXiv:2303.10516 (2023).

# Installation

The package will be submitted to Bioconductor. 

Currently, the package can be installed via GitHub

``` r
# install.packages("devtools")
devtools::install_github("ShuoStat/findIPs")
```
# Future work

The present version of findIPs can rank features based on i) t.test, univariate cox model, log2fc, and kruskal test; ii) customized ranking criteria. However, there is currently no direct link between findIPs and widely used differential expression methods like Deseqs or Limma. Our next task is to establish this connection so that these popular packages can be integrated into findIPs.
