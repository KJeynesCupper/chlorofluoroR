#' Summarise the FvFm and PSII values for each plant line.
#'
#' @description Instead of looking at the distribution of the FvFm or PSII
#' over multiple light pulses across time, we can calculate a mean. Here we plot
#' the mean for these variables as a bar plot with error bars representing SD.
#' As before, the colour is determined by the BASTA selection result.
#'
#'
#'@param data data.frame or list of data.frame; data output from
#'computeFvFm_PSII[computeFvFm_PSII()]
#'@param output_location path; location to save output files
#'@param width numeric; pdf width size. deafult is 18
#'@param height numeric;pdf height size. deafult is 25
#'
#' @return Output is also saved as an excel document
#' (one sheet per plate and variable). See output as files
#' "chlorofluoro_plot_3.pdf" and "chlorofluoro_dataset_3.xlsx"
#'
#'
#' @examples
#' data("step_one")
#' output_location <- tempdir()
#' step_three <- chlorofluor_summary(step_one, output_location)
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



summarise_chlorofluoro <- function(data,
                                   output_location,
                                   width= 18,
                                   height = 25){

  if (.function5(data)){ # for a  nested list

    out_summary <- mapply(.function4,data,SIMPLIFY = FALSE)
    # save data
    .function3(out_summary, file.path(output_location, "chlorofluoro_dataset_3.xlsx"))
    message("Data saved to ", file.path(output_location, "chlorofluoro_dataset_3.xlsx"))

    store_plots2 <- list()

    for (i in 1:length(out_summary)) {
      plate <- out_summary[[i]]
      plate_fvfm <- plate$FvFm
      plate_PSII <- plate$PSII
      val <- names(out_summary[i])

      plant_colours <- plate_fvfm %>%
        dplyr::group_by(colour) %>%
        dplyr::mutate(col_index = row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(color_value = case_when(
          colour == "green" ~ custom_greens[col_index],
          colour == "red" ~ custom_reds[col_index]
        )) %>%
        dplyr::select(Plant_ID, color_value)


      plate_fvfm <-  dplyr::left_join(plate_fvfm, plant_colours, by = "Plant_ID")%>%
        dplyr::mutate(order_val = as.numeric(sub(".*-", "", Plant_ID))) %>%
        dplyr::mutate(Plant_ID = factor(Plant_ID, levels = Plant_ID[order(order_val)]))

      plate_PSII <-  dplyr::left_join(plate_PSII, plant_colours, by = "Plant_ID")%>%
        dplyr::mutate(order_val = as.numeric(sub(".*-", "", Plant_ID))) %>%
        dplyr::mutate(Plant_ID = factor(Plant_ID, levels = Plant_ID[order(order_val)]))

      # Create a named vector for scale_color_manual
      color_vector <- plant_colours$color_value
      names(color_vector) <- plant_colours$Plant_ID

      p3 <- ggplot2::ggplot(plate_fvfm, aes(x = Plant_ID, y = FvFm_Mean, color = Plant_ID, fill = Plant_ID)) +
        ggplot2::scale_color_manual(values = color_vector) +
        ggplot2::scale_fill_manual(values = color_vector) +
        ggplot2::geom_bar(stat = "identity",  width = 0.6) +
        ggplot2::geom_errorbar(aes(ymin = FvFm_Mean - FvFm_SD, ymax = FvFm_Mean + FvFm_SD),width = 0.2, color = "black") +
        ggplot2::labs(y = "Fv/Fm", x = "Plant_ID", title = paste0(val, " Fv/Fm Mean with SD")) +
        ggplot2::theme_bw() +
        plotTheme+
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      p4 <- ggplot2::ggplot(plate_PSII, aes(x = Plant_ID, y = PSII_Mean, color = Plant_ID, fill = Plant_ID)) +
        ggplot2::scale_color_manual(values = color_vector) +
        ggplot2::scale_fill_manual(values = color_vector) +
        ggplot2::geom_bar(stat = "identity",  width = 0.6) +
        ggplot2::geom_errorbar(aes(ymin = PSII_Mean - PSII_SD, ymax = PSII_Mean + PSII_SD),width = 0.2, color = "black") +
        ggplot2::labs(y = "Fv/Fm", x = "Plant_ID", title = paste0(val, " PSII Mean with SD")) +
        ggplot2::theme_bw() +
        plotTheme+
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))


      store_plots2 <- append(store_plots2, list(p3, p4))

    }

    #save
    big_plot2 <- patchwork::wrap_plots(store_plots2, ncol = 2) # choose number of columns
    pdf(file.path(output_location, "chlorofluoro_plot_3.pdf"), width= width, height = height)
    print(big_plot2)
    dev.off()

  }
  else {
    out_summary <- .function4(data)
    # save data
    .function3(out_summary, file.path(output_location, "chlorofluoro_dataset_3.xlsx"))
    message("Data saved to ", file.path(output_location, "chlorofluoro_dataset_3.xlsx"))


    #plot
    plate_fvfm <- out_summary$FvFm
    plate_PSII <- out_summary$PSII

    plant_colours <- plate_fvfm %>%
      dplyr::group_by(colour) %>%
      dplyr::mutate(col_index = row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(color_value = case_when(
        colour == "green" ~ custom_greens[col_index],
        colour == "red" ~ custom_reds[col_index]
      )) %>%
      dplyr::select(Plant_ID, color_value)


    plate_fvfm <- dplyr::left_join(plate_fvfm, plant_colours, by = "Plant_ID")%>%
      dplyr::mutate(order_val = as.numeric(sub(".*-", "", Plant_ID))) %>%
      dplyr::mutate(Plant_ID = factor(Plant_ID, levels = Plant_ID[order(order_val)]))

    plate_PSII <- dplyr::left_join(plate_PSII, plant_colours, by = "Plant_ID")%>%
      dplyr::mutate(order_val = as.numeric(sub(".*-", "", Plant_ID))) %>%
      dplyr::mutate(Plant_ID = factor(Plant_ID, levels = Plant_ID[order(order_val)]))

    # Create a named vector for scale_color_manual
    color_vector <- plant_colours$color_value
    names(color_vector) <- plant_colours$Plant_ID

    p3 <- ggplot2::ggplot(plate_fvfm, aes(x = Plant_ID, y = FvFm_Mean, color = Plant_ID, fill = Plant_ID)) +
      ggplot2::scale_color_manual(values = color_vector) +
      ggplot2::scale_fill_manual(values = color_vector) +
      ggplot2::geom_bar(stat = "identity",  width = 0.6) +
      ggplot2::geom_errorbar(aes(ymin = FvFm_Mean - FvFm_SD, ymax = FvFm_Mean + FvFm_SD),width = 0.2, color = "black") +
      ggplot2::labs(y = "Fv/Fm", x = "Plant_ID", title = paste0("Fv/Fm Mean with SD")) +
      ggplot2::theme_bw() +
      plotTheme+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    p4 <- ggplot2::ggplot(plate_PSII, aes(x = Plant_ID, y = PSII_Mean, color = Plant_ID, fill = Plant_ID)) +
      ggplot2::scale_color_manual(values = color_vector) +
      ggplot2::scale_fill_manual(values = color_vector) +
      ggplot2::geom_bar(stat = "identity",  width = 0.6) +
      ggplot2::geom_errorbar(aes(ymin = PSII_Mean - PSII_SD, ymax = PSII_Mean + PSII_SD),width = 0.2, color = "black") +
      ggplot2::labs(y = "Fv/Fm", x = "Plant_ID", title = paste0("PSII Mean with SD")) +
      ggplot2::theme_bw() +
      plotTheme+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    big_plot2 <- patchwork::wrap_plots(list(p3, p4), ncol = 2) # choose number of columns
    pdf(file.path(output_location, "chlorofluoro_plot_3.pdf"), width= width, height = height)
    print(big_plot2)
    dev.off()
    message("Plots saved to ", file.path(output_location, "chlorofluoro_plot_3.pdf"))


  }
}
