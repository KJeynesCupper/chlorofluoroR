#' Compute distribution of FvFm and PSII.
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
#' output_location = tempdir()
#' step_one <- computeFvFm_PSII(CF_demodata,
#' plate_names = names(CF_demodata),
#' layout,
#' smartsheet = selection_results,
#' copynumber,
#' output_location)
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom openxlsx write.xlsx
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 stat_summary
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 aes
computeFvFm_PSII <- function(data,
                                plate_names,
                                layout,
                                smartsheet,
                                copynumber,
                                output_location,
                                width= 20,
                                height = 21){

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




    # plot
    plate_fvfm <- out1$FvFm
    plate_PSII <- out1$PSII
    val<- plate

    plant_colours <- plate_fvfm %>%
      dplyr::distinct(Plant_ID, colour) %>%
      dplyr::group_by(colour) %>%
      dplyr::mutate(col_index = row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(color_value = case_when(
        colour == "green" ~ custom_greens[col_index],
        colour == "red" ~ custom_reds[col_index]
      )) %>%
      dplyr::select(Plant_ID, color_value)


    plate_fvfm <- dplyr::left_join(plate_fvfm, plant_colours, by = "Plant_ID")
    plate_PSII <- dplyr::left_join(plate_PSII, plant_colours, by = "Plant_ID")

    # Create a named vector for scale_color_manual
    color_vector <- plant_colours$color_value
    names(color_vector) <- plant_colours$Plant_ID

    p1 <- ggplot2::ggplot(plate_fvfm, ggplot2::aes(x = Time, y = FvFm, color = Plant_ID, linetype = BAR_copy)) +
      ggplot2::stat_summary(fun = mean, geom = "line",linewidth = 1 ) +
      ggplot2::stat_summary(fun.data = mean_se, geom = "errorbar", width = 1, linewidth = 1) +
      ggplot2::labs(x = "Time (min)", y = "Fv/Fm", title = paste0(val, " FvFm"),
           linetype="Copy number", colour="Plant ID") +
      ggplot2::scale_color_manual(values = color_vector) +
      ggplot2::theme_bw() +
      plotTheme

    p2 <- ggplot2::ggplot(plate_PSII, ggplot2::aes(x = Time, y = PhiPSII, color = Plant_ID, linetype = BAR_copy)) +
      ggplot2::stat_summary(fun = mean, geom = "line", linewidth =1 ) +
      ggplot2::stat_summary(fun.data = mean_se, geom = "errorbar", width = 2, linewidth = 1 ) +
      ggplot2::labs(x = "Time (min)", y = "PSII",  title = paste0(val, " PSII"),
           linetype="Copy number", colour="Plant ID") +
      ggplot2::scale_color_manual(values = color_vector) +
      ggplot2::theme_bw() +
      plotTheme
    plots <- list(p1, p2)

  }else {
    # Create an empty list to store the results
    out1 <- list()

    # Loop over each plate name
    for (plate in plate_names) {
      out1[[plate]] <- .function1(
        data = data[[plate]],
        plate = plate,
        layout = layout,
        smartsheet = smartsheet,
        copynumber = copynumber
      )
    }
    # save data
    .function3(out1, file.path(output_location, "chlorofluoro_dataset_1.xlsx"))
    message("Data saved to ", file.path(output_location, "chlorofluoro_dataset_1.xlsx"))
    # plot!
    plots <- .function2(out1)
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

