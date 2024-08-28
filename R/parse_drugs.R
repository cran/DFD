#' Filter the drug based on the tested cell-line
#'
#'The function is used to re-rank drugs based on their targets
#'
#' @usage get_pert_by_type(perts, perts_type = c("cancer", "Normal"), high_targets = TRUE)
#'
#' @param perts drugs data frame returned by `get_drugs` function
#' 
#' @param perts_type type of drug, whether it's from a normal or cancerous cell line 
#' 
#' @param high_targets Boolean paramter to rank their drugs per the number of targets
#' 
#' @return re-ranked drug data frame based on their number of targets and cell line
#'
#' @export
#'
#' @author Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}
#'
get_pert_by_type <- function(perts, perts_type = c("cancer", "Normal"), high_targets = TRUE){
  selected_perts <- perts[perts$cell_type %in% perts_type, ] 
  selected_perts <- selected_perts[order(selected_perts$target_count, decreasing = high_targets),]
  return(selected_perts)
}
#' Parse the perturbagens to find out their frequency of occurrence.
#'
#' The function is used to parse drugs based on their targets
#'
#' @usage parse_perts(perts)
#'
#' @param perts drugs data frame returned by `get_drugs` function
#' 
#' @return re-ranked drug data frame based on their number of targets and cell line
#'
#' @export
#'
#' @author Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}
parse_perts <- function(perts) {
  perts_df <- gsub("^\\s+", "", unlist(strsplit(as.character(na.omit(perts$MOAss)), ";")))
  occurences <- as.data.frame(sort(table(perts_df), decreasing = TRUE))
  colnames(occurences) <- c("Function", "Count")
  occurences$`%` <- percent(occurences$Count / nrow(perts))
  return(occurences)
}
