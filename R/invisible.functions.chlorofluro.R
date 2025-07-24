#------------------------------------------------------------#
# Title:  Invisible functions                                #
# Author: Katie Jeynes-Cupper (kejc@illinois.edu)            #
# Date:   25th June 2025                                     #
#------------------------------------------------------------#

plotTheme <- ggplot2::theme(
  strip.background = ggplot2::element_blank(),
  strip.placement = "outside",
  panel.grid = ggplot2::element_blank(),
  axis.text.x = ggplot2::element_text(color="black", size = 16, margin = ggplot2::margin(t = 2, b = 4)),
  axis.text.y = ggplot2::element_text(color="black", size = 16, margin = ggplot2::margin(r = 5)) ,
  panel.grid.major.x = ggplot2::element_line( size=.1, color="grey", linetype = 2 ),
  panel.grid.major.y = ggplot2::element_line( size=.1, color="grey", linetype = 2 ),
  legend.position = "right",
  legend.box.margin=ggplot2::margin(20,20,20,20),
  legend.text = ggplot2::element_text(size=15, margin = ggplot2::margin(7,7,7,7)),
  legend.title = ggplot2::element_text(size = 16, face = "bold"),
   axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), size = 16, face = "bold"),
  axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), size = 16, face = "bold"),
  strip.text.x = ggplot2::element_text(size = 16,face="bold" , margin = ggplot2::margin(b = 5),  hjust = 0.5),
   strip.text.y = ggplot2::element_text(size = 16,face="bold" , margin = ggplot2::margin(l = 5),  vjust = 0.4, hjust = 0.2),
  plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
  legend.box = "horizontal")

plotTheme_v2 <- ggplot2::theme(
  strip.background = ggplot2::element_blank(),
  strip.placement = "outside",
  panel.grid = ggplot2::element_blank(),
  axis.text.x = ggplot2::element_text(color="black", size = 16, margin = ggplot2::margin(t = 2, b = 4)),
  axis.text.y = ggplot2::element_text(color="black", size = 16, margin = ggplot2::margin(r = 5)) ,
  panel.grid.major.x = ggplot2::element_line( size=.1, color="grey", linetype = 2 ),
  panel.grid.major.y = ggplot2::element_line( size=.1, color="grey", linetype = 2 ),
  legend.position = "right",
  legend.box.margin=ggplot2::margin(20,20,20,20),
  legend.text = ggplot2::element_text(size=12, margin = ggplot2::margin(7,7,7,7)),
  legend.title = ggplot2::element_text(size = 13, face = "bold"),
  axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), size = 16, face = "bold"),
  axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), size = 16, face = "bold"),
  strip.text.x = ggplot2::element_text(size = 16,face="bold" , margin = ggplot2::margin(b = 5),  hjust = 0.5),
  strip.text.y = ggplot2::element_text(size = 16,face="bold" , margin = ggplot2::margin(l = 5),  vjust = 0.4, hjust = 0.2),
  plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
  legend.box = "horizontal")

# add to these if need more!!!
custom_greens <- c("#C4E6C3FF",
                   "#96D2A4FF",
                   "#6DBC90FF",
                   "#4DA284FF",
                   "#36877AFF",
                   "#266B6EFF",
                   "#1D4F60FF",
                   "#006400",
                   "#228B22",
                   "#0B3D2E",
                   "#1C5D47",
                   "#2E7D60",
                   "#3F9D79",
                   "#5AAE8A",
                   "#497D63",
                   "#355F4B",
                   "#1E4635",
                   "#264D3D",
                   "#144035" )
custom_reds   <- c("#8B0000",
                   "#B22222",
                   "#DC143C",
                   "#FA8072",
                   "#F08080",
                   "darkred",
                   "#5B0000",
                   "#B14222",
                   "#4B1E24",
                   "#611A1E",
                   "#7C2F34",
                   "#8B3A3C",
                   "#A34747",
                   "#B35A5A",
                   "#66292B",
                   "#531E1F",
                   "#732D2D",
                   "#9C4F4F")



## hidden
.function1 <- function(data,
                       plate_names,
                       layout,
                       smartsheet,
                       copynumber){
  rownames(data) <- data$X
  data <- data[,-1]
  data_t <- as.data.frame(t(data))
  data_t$wells <- rownames(data_t)

  layout <- as.data.frame(layout[[plate_names]])
  data_anno <- merge(data_t, layout, by.x = "wells", by.y = "Well")
  data_anno <- data_anno %>%
    dplyr::select(`Construct_ID`, Plant_ID, Replicate, wells,
           QY_max, `Fv/Fm_L1`, `Fv/Fm_L2`, `Fv/Fm_L3`, `Fv/Fm_L4`, `QY_L1`, `QY_L2`, `QY_L3`, `QY_L4`)%>%
    dplyr::left_join(smartsheet, by = "Plant_ID")%>%
    dplyr::mutate(Selection = dplyr::if_else(`BASTA result` == "Positive", "pass", "fail"))

  # Reshape Fv/Fm
  fvfm_long <- data_anno %>%
    dplyr::select(Plant_ID, Replicate, dplyr::starts_with("Fv/Fm_L"), Selection) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Fv/Fm_L"), names_to = "Timepoint", values_to = "FvFm") %>%
    dplyr::mutate(Time = as.numeric(gsub("Fv/Fm_L", "", Timepoint)) * 10)

  # Reshape ΦPSII (QY)
  phipsii_long <- data_anno %>%
    dplyr::select(Plant_ID, Replicate, dplyr::starts_with("QY_L"), Selection) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("QY_L"), names_to = "Timepoint", values_to = "PhiPSII") %>%
    dplyr::mutate(Time = as.numeric(gsub("QY_L", "", Timepoint)) * 10)

  dflist <- list(FvFm =fvfm_long,
                 PSII = phipsii_long)
  # add copy number
  dflist <- lapply(dflist, function(df) {
    merge(df, copynumber, by = "Plant_ID", all.x = TRUE)
  })


  return(dflist)
}



### hidden
.function2 <- function(list_of_dfs){
  store_plots <- list()
  for (i in 1:length(list_of_dfs)) {
    # depending on input:
     if(.function5(list_of_dfs) == TRUE) {
      plate <- list_of_dfs[[i]]
    } else {
      plate <- list_of_dfs
    }
    plate <- list_of_dfs[[i]]
    plate_fvfm <- plate$FvFm
    plate_PSII <- plate$PSII
    val <- names(list_of_dfs[i])

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
      scale_color_manual(values = color_vector) +
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
    store_plots <- append(store_plots, list(p1, p2))
  }
  return(store_plots)
}

## hidden3. save data as excel. one sheet per df in nested list.
.function3 <- function(nested_list, file_path) {
  sheet_list <- list()

  for (plate_name in names(nested_list)) {
    plate_data <- nested_list[[plate_name]]

    if ("FvFm" %in% names(plate_data)) {
      sheet_list[[paste0(plate_name, "_FvFm")]] <- plate_data$FvFm
    }
    if ("PSII" %in% names(plate_data)) {
      sheet_list[[paste0(plate_name, "_PSII")]] <- plate_data$PSII
    }
  }
  # Write to Excel file
  openxlsx::write.xlsx(sheet_list, file = file_path, rowNames = FALSE)
}



.function4 <- function(data){
  fvfm <- data$FvFm
  PSII <- data$PSII

  # basta
  selection <- fvfm %>%
    dplyr::select(Plant_ID, Selection)%>%
    distinct()

  #fvfm >> Calculate means and sd and count technical reps
  fvfm_tech_reps <- as.data.frame(table(fvfm$Plant_ID))%>%
    dplyr::rename(Plant_ID = Var1, techreps = Freq)

  fvfm_mean_df<-stats::aggregate(fvfm$FvFm, list(fvfm$Plant_ID), mean)%>%
    dplyr::rename(Plant_ID = Group.1, FvFm_Mean = x)

  fvfm_sd_df<-stats::aggregate(fvfm$FvFm, list(fvfm$Plant_ID), sd)%>%
    dplyr::rename(Plant_ID = Group.1,FvFm_SD = x)

  summary_FvFm_BioReps <- merge(fvfm_mean_df, fvfm_sd_df, by = "Plant_ID")
  summary_FvFm_BioReps$Plant_ID <- as.factor(summary_FvFm_BioReps$Plant_ID)
  summary_FvFm_BioReps <- fvfm_tech_reps %>%
    dplyr::full_join(summary_FvFm_BioReps)%>%
    dplyr::left_join(selection, by = "Plant_ID")


  #PSII >> Calculate means and sd and count technical reps
  PSII_tech_reps <- as.data.frame(table(PSII$Plant_ID))%>%
    dplyr::rename(Plant_ID = Var1, techreps = Freq)

  PSII_mean_df<-stats::aggregate(PSII$PhiPSII, list(PSII$Plant_ID), mean)%>%
    dplyr::rename(Plant_ID = Group.1, PSII_Mean = x)

  PSII_sd_df<-stats::aggregate(PSII$PhiPSII, list(PSII$Plant_ID), sd)%>%
    dplyr::rename(Plant_ID = Group.1,PSII_SD = x)

  summary_PSII_BioReps <- merge(PSII_mean_df, PSII_sd_df, by = "Plant_ID")
  summary_PSII_BioReps$Plant_ID <- as.factor(summary_PSII_BioReps$Plant_ID)
  summary_PSII_BioReps <- PSII_tech_reps %>%
    dplyr::full_join(summary_PSII_BioReps)%>%
    dplyr::left_join(selection, by = "Plant_ID")


  out <- list(FvFm = summary_FvFm_BioReps,
              PSII = summary_PSII_BioReps)
  return(out)
}




## hidden5
.function5 <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  } else {
    return(any(sapply(x, function(el) is.list(el) && !is.data.frame(el))))
  }
}





.function6 <- function(list_of_dfs, colour_palette, label_size){
  store_plots <- list()

  for (i in 1:length(list_of_dfs)) {
    # depending on input:
    if(.function5(list_of_dfs) == TRUE) {
      plate <- list_of_dfs[[i]]
    } else {
      plate <- list_of_dfs
    }
    plate_fvfm <- plate$FvFm
    plate_PSII <- plate$PSII
    val <- names(list_of_dfs[i])

    if(is.null(colour_palette)){
      len <- length(unique(plate_fvfm$Plant_ID))
      colour_palette <- custom_greens[3:len]
    }


    plate_fvfm_summary <- plate_fvfm %>%
      dplyr::group_by(Time, BAR_copy, Selection, Plant_ID) %>%
      dplyr::summarise(
        FvFm_mean = mean(FvFm, na.rm = TRUE),
        FvFm_sd = sd(FvFm, na.rm = TRUE),
        FvFm_se = FvFm_sd / sqrt(n()),
        .groups = "drop"
      )

    plate_fvfm_labels <- plate_fvfm_summary %>%
      dplyr::group_by(Plant_ID) %>%
      dplyr::slice_max(Time, n = 1)

    plate_PSII_summary <- plate_PSII %>%
      dplyr::group_by(Time, BAR_copy, Selection, Plant_ID) %>%
      dplyr::summarise(
        PSII_mean = mean(PhiPSII, na.rm = TRUE),
        PSII_sd = sd(PhiPSII, na.rm = TRUE),
        PSII_se = PSII_sd / sqrt(n()),
        .groups = "drop"
      )

    plate_PSII_labels <- plate_PSII_summary %>%
      dplyr::group_by(Plant_ID) %>%
      dplyr::slice_max(Time, n = 1)



    p1 <- ggplot2::ggplot(plate_fvfm_summary,
                          ggplot2::aes(x = Time, y = FvFm_mean,group = Plant_ID,
                                   color = BAR_copy,linetype = Selection)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size =3) +
      ggplot2::scale_color_manual(values = colour_palette)+
      ggplot2::labs(x = "Time (min)", y = "Fv/Fm", title = paste0(val, " FvFm"),
                    linetype="Selection screen", colour="Copy No.") +
      ggplot2::theme_bw() +
      plotTheme_v2+
      ggplot2::scale_linetype_manual(values = c("pass" = "solid", "fail" = "dashed"))+
      ggplot2::theme(legend.box = "vertical", legend.key.width = grid::unit(1, "cm"))+
      ggrepel::geom_text_repel(data = plate_fvfm_labels,
                               ggplot2::aes(label = Plant_ID),size = label_size,
                               show.legend = FALSE, nudge_x = 7)




    p2 <- ggplot2::ggplot(plate_PSII_summary, ggplot2::aes(x = Time, y = PSII_mean,
                                                  group = Plant_ID,
                                                  color = BAR_copy,
                                                  linetype = Selection)) +
      ggplot2::geom_line(size =1) +
      ggplot2::geom_point(size =3) +
      ggplot2::scale_color_manual(values = colour_palette)+
      ggrepel::geom_text_repel(data = plate_PSII_labels,
                               ggplot2::aes(label = Plant_ID),size = label_size,
                               show.legend = FALSE, nudge_x = 7)+
      ggplot2::labs(x = "Time (min)", y = "PSII", title = paste0(val, " PSII"),
                    linetype="Selection screen", colour="Copy No.") +
      ggplot2::scale_linetype_manual(values = c("pass" = "solid", "fail" = "dashed"))+
      ggplot2::theme_bw() +
      plotTheme+
      ggplot2::theme(legend.box = "vertical", legend.key.width = grid::unit(1, "cm"))

    store_plots <- append(store_plots, list(p1, p2))
  }
  return(store_plots)
}








# hidden 7

.function7 <- function(list_of_dfs, colour_palette){
  store_plots <- list()

  if(is.null(colour_palette)){


    # Extract all data frames in the nested list that contain "FvFm"
    result <- lapply(list_of_dfs, function(sublist) {
      Filter(function(df) "FvFm" %in% names(df), sublist)
    })

    # Flatten the result if you want a single list of matching dfs
    result_flat <- do.call(c, result)
    final_df <- do.call(rbind, result_flat)

    len <- length(unique(final_df$BAR_copy))
    colour_palette <-viridis::viridis(len, option = "D")
    #grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral"))(len)
  }


  for (i in 1:length(list_of_dfs)) {
    # depending on input:
    if(.function5(list_of_dfs) == TRUE) {
      plate <- list_of_dfs[[i]]
    } else {
      plate <- list_of_dfs
    }

    plate_fvfm <- plate$FvFm
    plate_PSII <- plate$PSII
    val <- names(list_of_dfs[i])

    plate_fvfm_summary <- plate_fvfm %>%
      dplyr::group_by(Time, BAR_copy, Selection, Plant_ID) %>%
      dplyr::summarise(
        FvFm_mean = mean(FvFm, na.rm = TRUE),
        FvFm_sd = sd(FvFm, na.rm = TRUE),
        FvFm_se = FvFm_sd / sqrt(n()),
        .groups = "drop"
      )

    plate_PSII_summary <- plate_PSII %>%
      dplyr::group_by(Time, BAR_copy, Selection, Plant_ID) %>%
      dplyr::summarise(
        PSII_mean = mean(PhiPSII, na.rm = TRUE),
        PSII_sd = sd(PhiPSII, na.rm = TRUE),
        PSII_se = PSII_sd / sqrt(n()),
        .groups = "drop"
      )


    # Prepare colors (as before)
    plant_colors <- plate_fvfm_summary %>%
      distinct(Plant_ID, BAR_copy) %>%
      arrange(Plant_ID) %>%
      pull(BAR_copy) %>%
      as.factor()
    # set names
    names(colour_palette) <- levels(plant_colors)

    # Prepare linetypes and shapes based on BASTA
    plant_basta <- plate_fvfm_summary %>%
      distinct(Plant_ID, Selection) %>%
      arrange(Plant_ID)

    linetype_values <- c("pass" = "solid", "fail" = "dashed")
    shape_values <- c("pass" = 16, "fail" = 15)

    plant_id_colors <- plate_fvfm_summary %>%
      distinct(Plant_ID, BAR_copy) %>%
      mutate(color = colour_palette[BAR_copy]) %>%
      arrange(Plant_ID) %>%
      pull(color)
    names(plant_id_colors) <- plate_fvfm_summary %>%
      distinct(Plant_ID) %>% pull(Plant_ID)

    plant_id_linetypes <- plant_basta %>%
      mutate(lt = linetype_values[Selection]) %>%
      arrange(Plant_ID) %>%
      pull(lt)
    names(plant_id_linetypes) <- plant_basta$Plant_ID

    plant_id_shapes <- plant_basta %>%
      mutate(sh = shape_values[Selection]) %>%
      arrange(Plant_ID) %>%
      pull(sh)
    names(plant_id_shapes) <- plant_basta$Plant_ID


    # Create dummy data for legends
    barcopy_legend_df <- plate_fvfm_summary %>%
      distinct(BAR_copy)

    basta_legend_df <- data.frame(Selection = factor(c("pass", "fail"), levels = c("pass", "fail")))

    p1 <- ggplot2::ggplot(plate_fvfm_summary,
                          ggplot2::aes(x = Time, y = FvFm_mean,
                                       group = Plant_ID,
                                       color = Plant_ID,
                                       linetype = Plant_ID,
                                       shape = Plant_ID)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3) +

      # Plant_ID scales (actual aesthetics used in main plot)
      ggplot2::scale_color_manual(values = plant_id_colors) +
      ggplot2::scale_linetype_manual(name = "Selection screen",,values = plant_id_linetypes) +
      ggplot2::scale_shape_manual(values = plant_id_shapes) +

      # Dummy geom for BAR_copy legend using `fill`
      ggplot2::geom_point(data = barcopy_legend_df,
                          ggplot2::aes(x = Inf, y = Inf, fill = BAR_copy),
                          shape = NA, size = NA, inherit.aes = FALSE, show.legend = TRUE) +

      ggplot2::scale_fill_manual(name = "Copy No.", values = colour_palette,
                                 guide = ggplot2::guide_legend(override.aes = list(shape = 21, size = 4))) +
      ggplot2::geom_point(data = basta_legend_df,
                          ggplot2::aes(x = Inf, y = Inf, alpha = Selection),
                          shape = NA, size = NA, inherit.aes = FALSE, show.legend = TRUE) +

      ggplot2::scale_alpha_manual(name = "Selection screen",
                                  values = c("pass" = 1, "fail" = 1),  # different alpha just to trick legend
                                  guide = ggplot2::guide_legend(override.aes = list(
                                    shape = shape_values,
                                    linetype = linetype_values,
                                    color = "black",
                                    size = 3))) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "Plant_ID",
                                           override.aes = list(
                                             color = unname(plant_id_colors),
                                             shape = 16, linetype = "solid"), ncol = 2, order = 1),
                      linetype = ggplot2::guide_legend(title = "Plant_ID",ncol = 2, order = 1),
                      shape = ggplot2::guide_legend(title = "Plant_ID", ncol = 2, order = 1))+
      ggplot2::labs(x = "Time (min)", y = "Fv/Fm",title = paste0(val, " FvFm"))+
      ggplot2::theme_bw()+
      plotTheme+
      ggplot2::theme(legend.box = "verticle")




    p2 <-  ggplot2::ggplot(plate_PSII_summary,
                           ggplot2::aes(x = Time, y = PSII_mean,
                                        group = Plant_ID,
                                        color = Plant_ID,
                                        linetype = Plant_ID,
                                        shape = Plant_ID)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3) +

      # Plant_ID scales (actual aesthetics used in main plot)
      ggplot2::scale_color_manual(values = plant_id_colors) +
      ggplot2::scale_linetype_manual(values = plant_id_linetypes) +
      ggplot2::scale_shape_manual(values = plant_id_shapes) +

      # Dummy geom for BAR_copy legend using `fill`
      ggplot2::geom_point(data = barcopy_legend_df,
                          ggplot2::aes(x = Inf, y = Inf, fill = BAR_copy),
                          shape = NA, size = NA, inherit.aes = FALSE, show.legend = TRUE) +

      ggplot2::scale_fill_manual(name = "Copy No.", values = colour_palette,
                                 guide = guide_legend(override.aes = list(shape = 21, size = 4))) +
      ggplot2::geom_point(data = basta_legend_df,
                          ggplot2::aes(x = Inf, y = Inf, alpha = Selection),
                          shape = NA, size = NA, inherit.aes = FALSE, show.legend = TRUE) +

      ggplot2::scale_alpha_manual(name = "Selection screen",
                                  values = c("pass" = 1, "fail" = 1),  # different alpha just to trick legend
                                  guide = guide_legend(override.aes = list(
                                    shape = shape_values,
                                    linetype = linetype_values,
                                    color = "black",
                                    size = 3))) +
      ggplot2::guides(color = guide_legend(title = "Plant_ID",
                                           override.aes = list(
                                             color = unname(plant_id_colors),
                                             shape = 16, linetype = "solid"), ncol = 2, order = 1),
                      linetype = guide_legend(title = "Plant_ID",ncol = 2, order = 1),
                      shape = guide_legend(title = "Plant_ID", ncol = 2, order = 1))+
      ggplot2::labs(x = "Time (min)", y = "PSII",title = paste0(val, " PSII"))+
    theme_bw()+
      plotTheme+
      theme(legend.box = "verticle")


    store_plots <- append(store_plots, list(p1, p2))
  }
  return(store_plots)
}




utils::globalVariables(c("plate_names", "Construct_ID", "Plant_ID", "Replicate",
                         "wells","QY_max","Fv/Fm_L1", "Fv/Fm_L2",
                         "Fv/Fm_L3","Fv/Fm_L4", "QY_L1", "QY_L2", "QY_L3",
                         "QY_L4", "BASTA result", "colour", "Timepoint",
                         "color_value", "FvFm","Bar_copy", "PhiPSII", "Var1",
                         "Freq","Group.1", "x", "sd", "plate_fvfm", "Selection",
                         "BAR_copy", "FvFm_Mean", "FvFm_SD", "FvFm_mean",
                         "FvFm_sd", "PSII_Mean", "PSII_SD",
                         "PSII_mean", "PSII_sd", "Time", "color", "smartsheet",
                         "sh", "lt", "order_val"))












