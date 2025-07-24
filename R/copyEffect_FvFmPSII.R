#' Copy number effect
#'
#' @description Organises the raw data output for 1 or more plates and plots the
#' distribution.
#'
#' @details
#' Takes as input a single dataframe or a list of dataframe. Save data as
#' chlorofluoro_dataset_1.xlsx and plots as chlorofluoro_plot_1.pdf in desired
#' directory.
#'
#'
#'
#'@param data data.frame or list of data.frame; from FluorCam software
#' (frames-numeric)
#'@param plate_names character; name of plates.
#'@param layout data.frame or list of data.frame; plate layout.
#'@param smartsheet data.frame; selection results, ie BASTA.
#'@param copynumber data.frame; copy number results.
#'@param output_location path; location to save output files
#'@param width numeric; pdf width size. deafult is 20.
#'@param height numeric;pdf height size. deafult is 21.
#'@param version numeric; version of plot, choose from 1, 2, and 3. Each returns
#'a slightly different way to plot the data.
#'Version 1 = coloured based on selection result, and linetype represents
#'copy number.
#'Version 2 = coloured based on copy number, and linetype represents
#'selection result with each plant labeled within the plot
#'
#'Version 3 = coloured based on copy number, and linetype/shape represents
#'selection result with each line labeled via the legend.
#'
#'Version 4 = Grouping plants based on copy number.
#'
#'@param colour_palette character; colour palette for ggplot. Default is Null,
#'when used plots use packages default green palette (n=19).
#'
#' @return Returns a list of dataframes, where each plates has a dataframe for
#' FvFm and PSII. Saves an excel file containing all data and a plot for
#' FvFm and PSII.
#'
#' @examples
#' data("CF_demodata")
#' data("layout")
#' data("selection_results")
#' data("copynumber")
#'
#' plate_names <- names(CF_demodata)
#' smartsheet <- selection_results
#' output_location <-  tempdir()
#'
#' step_one <- computeFvFm_PSII(CF_demodata,plate_names,layout,smartsheet ,
#' copynumber,output_location, version =4 )
#'
#' @export
#' @import openxlsx
#' @import patchwork
#' @import dplyr
#' @import ggplot2
#' @import ggrepel
#' @import viridis
copyEffect_FvFmPSII <- function(data,
                             plate_names,
                             layout,
                             smartsheet,
                             copynumber,
                             output_location,
                             colour_palette = NULL,
                             version = 1,
                             width= 20,
                             height = 25){

  if(is.data.frame(data)){
    out1 <- .function1(data,
                       plate_names,
                       layout,
                       smartsheet,
                       copynumber)

    # save data
    excel <- list(FvFm = out1$FvFm,PSII = out1$PSII)
    openxlsx::write.xlsx(excel, file = file.path(output_location, "chlorofluoro_dataset_1.xlsx"), rowNames=F)
    message("Data saved to ", file.path(output_location, "chlorofluoro_dataset_1.xlsx"))

    plots <- switch(as.character(version),
                    "1" = .function2(out1),
                    "2" = .function6(out1, colour_palette),
                    .function7(out1))

  }else {
    # Create an empty list to store the results
    out1 <- list()

    # Loop over each plate name
    for (plate in plate_names) {
      out1[[plate]] <- .function1(
        data = data[[plate]],
        plate_names = plate,
        layout = layout,
        smartsheet = smartsheet,
        copynumber = copynumber
      )
    }
    # save data
    .function3(out1, file.path(output_location, "chlorofluoro_dataset_1.xlsx"))
    message("Data saved to ", file.path(output_location, "chlorofluoro_dataset_1.xlsx"))

    # plot!
    plots <- switch(as.character(version),
                    "1" = .function2(out1),
                    "2" = .function6(out1, colour_palette),
                    .function7(out1, colour_palette))
  }

  # save plots
  big_plot <- patchwork::wrap_plots(plots, ncol = 2) # choose number of columns

  pdf(file.path(output_location, "chlorofluoro_plot.pdf"), width= width, height = height)
  print(big_plot)

  dev.off()

  message("Plots saved to ", file.path(output_location, "chlorofluoro_plot_1.pdf"))
  message("Returned data")

  return(out1)
}


