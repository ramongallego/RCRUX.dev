#' Function to make and save histograms as pdfs
#' @export 
# I'm not fully convinced this is necessary, so I am not writing documentation for it.
make_hist_save_pdf <- function(infile, description,  file_out_dir, Metabarcode_name){
  grDevices::pdf(paste0(file_out_dir, Metabarcode_name, description,".pdf"), height = 4, width = 6, onefile=T)
  plot <- graphics::hist(infile)
  print(plot)
  grDevices::dev.off()
}