#' Flatten correlation matrix
#'
#' Flatten correlation matrix into a 2D dataframe based on \code{\link[www.sthda.com/french/wiki/matrice-de-correlation-formattage-et-visualisation]{http://www.sthda.com/french/wiki/matrice-de-correlation-formattage-et-visualisation}}
#'
#' @param cormat A correlation matrix
#' @param pmat A p-value matrix
#'
#' @return A dataframe with correlation and p-values for each pairwise comparison
#' @export
#'
#' @importFrom ggcorrplot cor_pmat
#' @importFrom data.table data.table
#'
#' @examples # Dataset generation
#' data_test = data.frame(sapply(c(1:10),function(x) rnorm(10)))
#'
#' # Correlation matrix calculation
#' cor_test = cor(data_test)
#'
#' # Associated p-values
#' cor_test_p = ggcorrplot::cor_pmat(data_test)
#'
#' # Flatten the correlation matrix
#' flat_cor_mat(cor_test, cor_test_p)
#'
flat_cor_mat <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.table(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
