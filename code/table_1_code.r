library(xtable)
table_1 <- xtable(tibble::tibble(
  Variable = c(
    "Strength of gene drive in females (e.g. \\textit{W}-shredding)",
    "Strength of gene drive in males (e.g. gene conversion)",
    "Cost of gene drive allele to female fecundity",
    "Cost of gene drive allele to male mating success",
    "Frequency of \\textit{W}-linked resistance mutations",
    "Frequency of \\textit{Z}-linked resistance mutations and NHEJ",
    "Frequency of autosomal resistance alleles",
    "Patchiness of the population",
    "Dispersal rate of males and females",
    "Global versus local density-dependence of female fecundity",
    "Contribution of males relative to females in density-dependence",
    "Number of gene drive carrier males released",
    "Release strategy: all in one patch, or global",
    "Fecundity of females at low population densities",
    "Shape of density dependence"
  ),
  `Parameter(s)` = c(
    "$p_{shred}$",
    "$p_{conv}$",
    "$c_f$",
    "$c_m$",
    "$\\mu_W$",
    "$\\mu_Z$ and $p_{nhej}$",
    "$\\mu_A$ and $\\mu_B$",
    "$k$",
    "$x_m$ and $x_f$",
    "$\\psi$",
    "$\\delta$",
    "$n_{release}$",
    "-",
    "$r$",
    "$\\alpha$"
  ),
  Outcome = 1
))

print_table1 <- function(){
  print(table_1, include.rownames=FALSE,
        comment = FALSE,
        sanitize.text.function = function(x){x})
}
