weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =
                                       na.rm)
}
# Source documentation
# weighted.var function from: https://stat.ethz.ch/pipermail/r-help/2008-July/168762.html
#   accessed Nov. 26, 2014, by Gavin Simpson gavin.simpson at ucl.ac.uk
# %~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%
# Dr. Gavin Simpson             [t] +44 (0)20 7679 0522
# ECRC, UCL Geography,          [f] +44 (0)20 7679 0565
# Pearson Building,             [e] gavin.simpsonATNOSPAMucl.ac.uk
# Gower Street, London          [w] http://www.ucl.ac.uk/~ucfagls/
#   UK. WC1E 6BT.                 [w] http://www.freshwaters.org.uk
# %~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%
# 
#   Thu Jul 24 16:27:15 CEST 2008
#   weighted.var() is via the usual formula and
#   weighted.var2() uses a running sums approach. The formulae for which are
#   both on the weighted mean entry page on wikipedia for example.
#   http://en.wikipedia.org/wiki/Weighted_arithmetic_mean