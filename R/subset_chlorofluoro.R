#' ubset for specific combinations
#'
#' @description We want to use this data to help us select for lines to carry to
#' T2. So, here we will filter for these desired traits. For example, we will
#' select for those with a copy number value of 4 and BASTA positive
#' (ie. resistant).
#'
#'
#'@param data data.frame or list of data.frame; data output from
#'computeFvFm_PSII[computeFvFm_PSII()]
#'@param output_location path; location to save output files
#'@param copyNumber numeric; copy number to select for
#'@param BASTA character; based on chosen value. Default is "positive"
#'@param width numeric; pdf width size. deafult is 15
#'@param height numeric;pdf height size. deafult is 5
#'
#' @return This outputs a plot and excel document in the same way as
#' computeFvFm_PSII() function. See output as files "chlorofluoro_plot_2.pdf" and
#' "chlorofluoro_dataset_2.xlsx"
#'
#' @examples
#' data("step_one")
#' output_location <- tempdir()
#' step_two <- subset_chlorofluoro(step_one,
#' output_location)
#'
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

subset_chlorofluoro <- function(data,
                               copyNumber = 4,
                               BASTA = "positive",
                               width= 15,
                               height = 5,
                               output_location){

  if(BASTA =="positive"){
    basta_result <- "green"
  }else{
    basta_result <- "red"
  }


  if(.function5(data) == TRUE) {

    # filter
    filtered_df <- lapply(data, function(x) {
      lapply(x, function(df) {
        dplyr::filter(df, BAR_copy == copyNumber,
                      colour == basta_result)
      })
    })

    # select for Fvfm values:
    combined_FvFm <- bind_rows(
      lapply(filtered_df, function(x) x[["FvFm"]]),
      .id = "plate")

    # select for PSII values:
    combined_PSII <- bind_rows(
      lapply(filtered_df, function(x) x[["PSII"]]),
      .id = "plate")


  } else{ # working with single plate
    filtered_df <-  lapply(data, function(x) {
      dplyr::filter(x, BAR_copy == copyNumber,
                    colour == basta_result)
    })
    combined_FvFm <- filtered_df$FvFm
    combined_PSII <- filtered_df$PSII
  }


  p1 <- ggplot2::ggplot(combined_FvFm, aes(x = Time, y = FvFm, color = Plant_ID)) +
    ggplot2::stat_summary(fun = mean, geom = "line",linewidth = 1.2 ) +
    ggplot2::stat_summary(fun.data = mean_se, geom = "errorbar", width = 1, linewidth = 1) +
    ggplot2::labs(x = "Time (min)", y = "Fv/Fm", title =paste0(copyNumber, " Copy Plants:FvFm"),
         linetype="Copy number", colour="Plant ID") +
    ggplot2::theme_bw() +
    plotTheme

  p2 <- ggplot2::ggplot(combined_PSII, aes(x = Time, y = PhiPSII, color = Plant_ID)) +
    ggplot2::stat_summary(fun = mean, geom = "line", linewidth =1.2 ) +
    ggplot2::stat_summary(fun.data = mean_se, geom = "errorbar", width = 2, linewidth = 1 ) +
    ggplot2::labs(x = "Time (min)", y = "PSII", title =paste0(copyNumber, " Copy Plants:PSII"),
         linetype="Copy number", colour="Plant ID") +
    ggplot2::theme_bw() +
    plotTheme

  list_plots <- list(p1, p2)
  big_plot <- patchwork::wrap_plots(list_plots, ncol = 2) # choose number of columns

  #dir.create("../3_plots")
  pdf(file.path(output_location, "chlorofluoro_plot_2.pdf"), width= width, height = height)
  print(big_plot)
  dev.off()

  # save data:
  dataset_sv <- list(FvFm = combined_FvFm,
                     PSII = combined_PSII)
  openxlsx::write.xlsx(dataset_sv, file.path(output_location, "chlorofluoro_dataset_2.xlsx"), rowNames=F)
  message("Data saved to ", file.path(output_location, "chlorofluoro_dataset_2.xlsx"))

}
