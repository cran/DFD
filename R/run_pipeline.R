#' Run the main pipeline for getting drugs from differentail expression profile
#'
#'The function is used to run the main pipeline by extracting the drug list given differential expressed genes
#'
#' @usage run_pipeline(degs_path, output_path = NULL)
#'
#' @param degs_path path to csv file containing degs see example file at https://raw.githubusercontent.com/MohmedSoudy/datasharing/master/differential-expression.csv
#'
#' @param output_path absolute path to output directory
#'
#' @return significant drug data frame after the re-ranking step
#'
#' @export
#'
#' @author Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}
#'
run_pipeline <- function(degs_path, output_path = NULL){

  #Read the DEGs into lists
  degs <- read_id(degs_path)
  #Process the DEGs to be prepared for CMAP search
  degs_processed <- prepare_ids(degs[[1]], degs[[2]])
  #Get the list of drugs
  perts <- get_drugs(degs_processed[[1]], degs_processed[[2]])
  if (dim(perts)[1] < 0){
   message("There is No significant drugs associated with your query.")
    return()
  }
  processed_perts <- filter_drugs(perts)
  
  cell_line_types <- data.frame(
    cell_line = c("VCAP", "SKBR3", "SKB", "PHH", "PC3", "NPC", "NKDBA", "NEU", 
                  "MDAMB231", "MCF7", "MCF10A", "HUH7", "HT29", "HS578T",
                  "HEPG2", "HEK293T", "HCC515", "HA1E", "FIBRNPC",
                  "BT20", "ASC", "A549", "A375"),
    cell_type = c("cancer", "cancer", "cancer", "Normal", "cancer", "cancer",
                  "Normal", "cancer", "cancer", "cancer", "Normal", "cancer",
                  "cancer", "cancer","cancer", "Normal", "cancer", "Normal",
                  "Normal", "cancer", "Normal", "cancer", "cancer"),
    
    tissue = c("Prostate", "Breast", "Breast", "Liver", "Prostate",
               "Nasopharynx", "Bronchial", "Breast", "Breast",
               "Breast", "Breast", "Liver", "Colon", "Breast",
               "Liver", "kidney", "Lung", "Endometrial", "Brain",
               "Breast", "Adipose", "Lung", "Skin")
  )
  
  processed_perts <- merge(processed_perts, cell_line_types, by.x = "cell", by.y = "cell_line")
  processed_perts$target_count <- processed_perts$target_count + 1 
  if(!is.null(output_path)){
    write.csv(processed_perts, file = paste0(output_path, "/Predicted_drugs.csv"), row.names = F)
  }
  return(processed_perts)
  }
