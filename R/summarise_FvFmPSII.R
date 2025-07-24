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
#'@param copynumber data.frame; copy number results.
#'@param output_location path; location to save output files
#'@param width numeric; pdf width size. deafult is 18
#'@param height numeric;pdf height size. deafult is 25
#'@param colour_palette character; colour palette for ggplot. Default is NULL,
#'when used plots use packages default green palette (n=19).
#'
#' @return Output is also saved as an excel document
#' (one sheet per plate and variable). See output as files
#' "chlorofluoro_plot_3.pdf" and "chlorofluoro_dataset_3.xlsx"
#'
#'
#' @examples
#' data("step_one")
#' data("selection_results")
#' data("copynumber")
#' output_location <- tempdir()
#' step_three <- summarise_FvFmPSII(step_one, copynumber, output_location)
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
#' @importFrom ggplot2 mean_se
#' @import ggpubr
#' @import grDevices

summarise_FvFmPSII <- function(data,copynumber,
                                 output_location,
                               colour_palette =NULL,
                               width= 25, height = 25){

  if (.function5(data) == TRUE){ # for a  nested list

    out_summary <- mapply(.function4, data, SIMPLIFY = FALSE)
    # save data
    .function3(out_summary, file.path(output_location, "chlorofluoro_dataset_3.xlsx"))
    message("Data saved to ", file.path(output_location, "chlorofluoro_dataset_3.xlsx"))


    if(is.null(colour_palette)){
      # Extract all data frames in the nested list that contain "FvFm"
      result <- lapply(data, function(sublist) {
        Filter(function(df) "FvFm" %in% names(df), sublist)
      })
      # Flatten the result if you want a single list of matching dfs
      result_flat <- do.call(c, result)
      final_df <- do.call(rbind, result_flat)

      len <- length(unique(final_df$BAR_copy))
      colour_palette <-viridis::viridis(len, option = "D")
    }

    # which plate has the most copy numbers:
    copyno_p1 <- lapply(data, function(sublist) {
      Filter(function(df) "FvFm" %in% names(df), sublist)
    })
    copyno_p2 <- do.call(c, copyno_p1)
    copyno_p3 <- sapply(copyno_p2, function(df) length(unique(df$BAR_copy)))
    which_max <- which.max(copyno_p3)

    store_plots2 <- list()

    for (i in 1:length(out_summary)) {
      plate <- out_summary[[i]]
      plate_fvfm <- plate$FvFm
      plate_PSII <- plate$PSII
      val <- names(out_summary[i])


      plate_fvfm <-  plate_fvfm%>%
        dplyr::mutate(order_val = as.numeric(sub(".*-", "", Plant_ID))) %>%
        dplyr::mutate(Plant_ID = factor(Plant_ID, levels = Plant_ID[order(order_val)]))%>%
        dplyr::inner_join(copynumber,by = "Plant_ID")

      plate_PSII <-  plate_PSII%>%
        dplyr::mutate(order_val = as.numeric(sub(".*-", "", Plant_ID))) %>%
        dplyr::mutate(Plant_ID = factor(Plant_ID, levels = Plant_ID[order(order_val)]))%>%
        dplyr::inner_join(copynumber,by = "Plant_ID")

      # Create a named vector for scale_color_manual
      # Prepare colors (as before)
      plant_colors <- plate_fvfm %>%
        distinct(Plant_ID, BAR_copy) %>%
        arrange(Plant_ID) %>%
        pull(BAR_copy) %>%
        as.factor()
      # set names
      names(colour_palette) <- levels(plant_colors)


      p3 <- ggplot2::ggplot(plate_fvfm, ggplot2::aes(x = Plant_ID,
                                                     y = FvFm_Mean,
                                                     color = Selection,
                                                     linetype = Selection,
                                                     fill = BAR_copy)) +
        ggplot2::geom_bar(stat = "identity",  width = 0.6) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = FvFm_Mean - FvFm_SD, ymax = FvFm_Mean+ FvFm_SD),width = 0.2, color = "black") +
        ggplot2::scale_color_manual(values = c("pass" = "black", "fail" = "black")) +
      ggplot2::scale_linetype_manual(values = c("pass" = "solid", "fail" = "dashed")) +
        ggplot2::scale_fill_manual(values = colour_palette) +
        ggplot2::labs(y = "Fv/Fm", x = "Plant_ID",
                      title = paste0(val, " Fv/Fm Mean with SD"),
                      fill = "Copy No.",
                      linetype = "Selection screen") +
        ggplot2::theme_bw() +
        ggplot2::guides(color = "none",
                        linetype = ggplot2::guide_legend(override.aes = list(fill = NA,
                                                                    col = "black")))+
        plotTheme+
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))+
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       legend.box = "vertical")

      p4 <- ggplot2::ggplot(plate_PSII, ggplot2::aes(x = Plant_ID,
                                                     y = PSII_Mean,
                                                     color = Selection,
                                                     linetype = Selection,
                                                     fill = BAR_copy)) +
        ggplot2::geom_bar(stat = "identity",  width = 0.6) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = PSII_Mean - PSII_SD, ymax = PSII_Mean + PSII_SD),width = 0.2, color = "black") +
        ggplot2::labs(y = "PSII", x = "Plant_ID",
                      title = paste0(val, " PSII Mean with SD"),
                      fill = "Copy No.",
                      linetype = "Selection screen") +
        ggplot2::theme_bw() +
        ggplot2::guides(color = "none",
                        linetype = ggplot2::guide_legend(override.aes = list(fill = NA,
                                                                    col = "black")))+
        ggplot2::scale_color_manual(values = c("pass" = "black", "fail" = "black")) +
      ggplot2::scale_linetype_manual(values = c("pass" = "solid", "fail" = "dashed")) +
        ggplot2::scale_fill_manual(values = colour_palette) +
        plotTheme+
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))+
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       legend.box = "vertical")

      store_plots2 <- append(store_plots2, list(p3, p4))

    }
    p_legend <- which_max*2
    legend <- ggpubr::get_legend(store_plots2[[p_legend]] + ggplot2::theme(legend.position = "right", legend.box = "vertical"))

  } else {
    out_summary <- .function4(data)
    # save data
    .function3(out_summary, file.path(output_location, "chlorofluoro_dataset_3.xlsx"))
    message("Data saved to ", file.path(output_location, "chlorofluoro_dataset_3.xlsx"))


    #plot
    plate_fvfm <- out_summary$FvFm
    plate_PSII <- out_summary$PSII


    plate_fvfm <- dplyr::left_join(plate_fvfm, plant_colours, by = "Plant_ID")%>%
      dplyr::mutate(order_val = as.numeric(sub(".*-", "", Plant_ID))) %>%
      dplyr::mutate(Plant_ID = factor(Plant_ID, levels = Plant_ID[order(order_val)]))%>%
      dplyr::inner_join(copynumber,by = "Plant_ID")

    plate_PSII <- dplyr::left_join(plate_PSII, plant_colours, by = "Plant_ID")%>%
      dplyr::mutate(order_val = as.numeric(sub(".*-", "", Plant_ID))) %>%
      dplyr::mutate(Plant_ID = factor(Plant_ID, levels = Plant_ID[order(order_val)]))%>%
      dplyr::inner_join(copynumber,by = "Plant_ID")


    # Prepare colors

    if(is.null(colour_palette)){
      len <- length(unique(plate_fvfm$BAR_copy))
      colour_palette <-viridis::viridis(len, option = "D")
    }

    plant_colors <- plate_fvfm %>%
      distinct(Plant_ID, BAR_copy) %>%
      arrange(Plant_ID) %>%
      pull(BAR_copy) %>%
      as.factor()
    # set names
    names(colour_palette) <- levels(plant_colors)


    p3 <- ggplot2::ggplot(plate_fvfm, ggplot2::aes(x = Plant_ID,
                                                   y = FvFm_Mean,
                                                   color = Selection,
                                                   linetype = Selection,
                                                   fill = BAR_copy)) +
      ggplot2::geom_bar(stat = "identity",  width = 0.6) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = FvFm_Mean - FvFm_SD,
                                          ymax = FvFm_Mean+ FvFm_SD),width = 0.2,
                             color = "black") +
      ggplot2::labs(y = "Fv/Fm", x = "Plant_ID",
                    title = paste0(val, " Fv/Fm Mean with SD"),
                    fill = "Copy No.",
                    linetype = "Selection screen") +
      ggplot2::theme_bw() +
      ggplot2::guides(color = "none",
                      linetype = guide_legend(override.aes = list(fill = NA,
                                                                  col = "black")))+
      ggplot2::scale_color_manual(values = c("pass" = "black", "fail" = "black")) +
    ggplot2::scale_linetype_manual(values = c("pass" = "solid", "fail" = "dashed")) +
      ggplot2::scale_fill_manual(values = colour_palette) +
      plotTheme+
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     legend.box = "vertical")

    p4 <- ggplot2::ggplot(plate_PSII, ggplot2::aes(x = Plant_ID,
                                                   y = FvFm_Mean,
                                                   color = Selection,
                                                   linetype = Selection,
                                                   fill = BAR_copy)) +
      ggplot2::geom_bar(stat = "identity",  width = 0.6) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = FvFm_Mean - FvFm_SD,
                                          ymax = FvFm_Mean + FvFm_SD),width = 0.2, color = "black") +
      ggplot2::labs(y = "PSII", x = "Plant_ID",
                    title = paste0(val, " PSII Mean with SD"),
                    fill = "Copy No.",
                    linetype = "Selection screen") +
      ggplot2::theme_bw() +
      ggplot2::guides(color = "none",
                      linetype = guide_legend(override.aes = list(fill = NA,
                                                                  col = "black")))+
      ggplot2::scale_color_manual(values = c("pass" = "black", "fail" = "black")) +
    ggplot2::scale_linetype_manual(values = c("pass" = "solid", "fail" = "dashed")) +
      ggplot2::scale_fill_manual(values = colour_palette) +
      plotTheme+
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     legend.box = "vertical")

    store_plots2 <- list(p3, p4)
    legend <- get_legend(store_plots2[[1]] + theme(legend.position = "right", legend.box = "vertical"))
  }


  grDevices::pdf(file.path(output_location, "chlorofluoro_plot_3.pdf"), width= width, height = height )
  print(ggpubr::ggarrange(plotlist=store_plots2,
                          ncol = 2, nrow = 4,
                          common.legend = T,
                          legend.grob = legend,
                          legend = "right"))
  grDevices::dev.off()
}





