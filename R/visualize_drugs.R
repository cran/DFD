#' Visualise the number of perturbagens and the top activities
#'
#'The function is used to provide a handy visualization of the top activities 
#'
#' @usage plot_activities(perts, top = 10, directorypath = NULL)
#'
#' @param perts drugs data frame returned by `get_drugs` function or `get_pert_by_type` function
#' 
#' @param top Number of activities to be visualised (N) The default value is set to 10.
#' 
#' @param directorypath path to save the output figure 
#' 
#' @return re-ranked drug data frame based on their number of targets and cell line
#'
#' @export
#'
#' @author Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}

plot_activities <- function(perts, top = 10, directorypath = NULL){

perts_df <- na.omit(parse_perts(perts))[1:top, ]

perts_count <- ggplot(perts_df, aes(x = reorder(perts_df$Function, perts_df$Count), y = perts_df$Count)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  xlab("Activity/Function") + ylab("# Of Perturbagens") +
  theme_bw() + theme(text = element_text(size = 12, face = "bold", colour = "black"),
                     axis.text.x = element_text(vjust = 2)) +
  coord_flip()

summary_plot <- ggarrange(perts_count, tableGrob(perts_df, rows = NULL), ncol = 2, align = "hv")


if (!is.null(directorypath)) {
  ggsave(plot = summary_plot, filename = paste0(directorypath, 
                                                "//", "activity_summary.jpeg"), dpi = 320, width = 19, 
         height = 10)
  ggsave(plot = summary_plot, filename = paste0(directorypath, 
                                                "//", "activity_summary.tiff"), dpi = 320, width = 19, 
         height = 10)
}
plot(summary_plot)
}
