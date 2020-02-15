#' CreateOutputPlots
#'
#' Creates output plots
#'
#' @param mainResults Output of function \code{\link{PerformMainFit}}. Required.
#' @param bsResultsList Output of function \code{\link{PerformBootstrapFits}}. Optional.
#'
#' @return
#' list of ggplot2 objects
#'
#' @examples
#' \dontrun{
#' CreateOutputPlots(mainResults, bsResultsList)
#' }
#'
#' @export
CreateOutputPlots <- function(
  mainResults,
  bsResultsList = NULL
) {
  # CRAN checks
  `.` <- NULL
  Year <- NULL
  N_HIV_D <- NULL
  N_HIV_Obs_M <- NULL
  N_CD4_1_D <- NULL
  N_CD4_1_Obs_M <- NULL
  N_CD4_2_D <- NULL
  N_CD4_2_Obs_M <- NULL
  N_CD4_3_D <- NULL
  N_CD4_3_Obs_M <- NULL
  N_CD4_4_D <- NULL
  N_CD4_4_Obs_M <- NULL
  N_HIVAIDS_D <- NULL
  N_HIVAIDS_Obs_M <- NULL
  N_AIDS_D <- NULL
  N_AIDS_M <- NULL
  N_Inf_M <- NULL
  t_diag <- NULL
  N_Alive <- NULL
  N_Alive_Diag_M <- NULL
  N_Und <- NULL
  N_Und_Alive_p <- NULL
  D_Avg_Time <- D_Avg_Time_LB <- D_Avg_Time_UB <- NULL
  N_CD4_1_Obs_M_LB <- N_CD4_1_Obs_M_UB <- N_CD4_2_Obs_M_LB <- N_CD4_2_Obs_M_UB <-
    N_CD4_3_Obs_M_LB <- N_CD4_3_Obs_M_UB <- N_CD4_4_Obs_M_LB <- N_CD4_4_Obs_M_UB <- N_Dead_U <-
    N_HIV_Obs_M_LB <- N_HIV_Obs_M_UB <- Run <- NULL
  N_HIVAIDS_Obs_M_LB <- N_HIVAIDS_Obs_M_UB <- N_AIDS_M_LB <- N_AIDS_M_UB <- N_Inf_M_LB <-
    N_Inf_M_UB <- t_diag_LB <- t_diag_UB <- N_Alive_LB <- N_Alive_UB <- N_Alive_Diag_M_LB <-
    N_Alive_Diag_M_UB <- N_Und_LB <- N_Und_UB <- N_Und_Alive_p_LB <- N_Und_Alive_p_UB <- NULL

  colors <- c('#69b023', '#9d8b56', '#7bbcc0', '#ce80ce')
  fillColor <- '#d9d9d9'

  dt <- mainResults$MainOutputs[, .(
    Year,
    N_HIV_D, N_HIV_Obs_M,
    N_CD4_1_D, N_CD4_1_Obs_M,
    N_CD4_2_D, N_CD4_2_Obs_M,
    N_CD4_3_D, N_CD4_3_Obs_M,
    N_CD4_4_D, N_CD4_4_Obs_M,
    N_HIVAIDS_D, N_HIVAIDS_Obs_M,
    N_AIDS_D, N_AIDS_M,
    N_Inf_M,
    t_diag,
    D_Avg_Time,
    N_Alive, N_Alive_Diag_M, N_Und,
    N_Und_Alive_p
  )]
  if (!is.null(bsResultsList)) {
    conf <- bsResultsList$ConfBounds[
      Run == length(bsResultsList$IterResults),
      .(
        Year,
        N_HIV_Obs_M_LB, N_HIV_Obs_M_UB,
        N_CD4_1_Obs_M_LB, N_CD4_1_Obs_M_UB,
        N_CD4_2_Obs_M_LB, N_CD4_2_Obs_M_UB,
        N_CD4_3_Obs_M_LB, N_CD4_3_Obs_M_UB,
        N_CD4_4_Obs_M_LB, N_CD4_4_Obs_M_UB,
        N_HIVAIDS_Obs_M_LB, N_HIVAIDS_Obs_M_UB,
        N_AIDS_M_LB, N_AIDS_M_UB,
        N_Inf_M_LB, N_Inf_M_UB,
        t_diag_LB, t_diag_UB,
        D_Avg_Time_LB, D_Avg_Time_UB,
        N_Alive_LB, N_Alive_UB, N_Alive_Diag_M_LB, N_Alive_Diag_M_UB, N_Und_LB, N_Und_UB,
        N_Und_Alive_p_LB, N_Und_Alive_p_UB
      )
      ]
    dt <- merge(dt, conf, by = 'Year', all = TRUE)
  }

  GetPlotBase <- function() {
    list(
      scale_x_continuous(expand = c(0, 0)),
      scale_y_continuous(expand = c(0, 0)),
      theme_classic(),
      theme(
        plot.title = element_text(size = 12, face = 'plain'),
        text = element_text(size = 12, face = 'plain'),
        panel.grid = element_blank(),
        axis.line = element_line(colour = '#888888'),
        axis.ticks = element_line(colour = '#888888'),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)
      ),
      xlab('Year'),
      ylab('Count')
    )
  }

  GetPlot <- function(dt, dataVarName = NULL, modelVarName = NULL, title = NULL) {
    p <- ggplot(data = dt, aes(x = Year))
    if (!is.null(dataVarName)) {
      lineTypes <- c(Data = 'solid', Mean = 'dashed')
      p <- p + geom_line(aes_string(y = dataVarName, color = '"Data"', linetype = '"Data"'))
    } else {
      lineTypes <- c(Data = 'dashed', Mean = 'solid')
    }
    if (!is.null(modelVarName)) {
      p <- p +
        geom_line(aes_string(y = modelVarName, color = '"Mean"', linetype = '"Mean"'), size = 0.8)

      lbVarName <- sprintf('%s_LB', modelVarName)
      ubVarName <- sprintf('%s_UB', modelVarName)
      if (all(c(lbVarName, ubVarName) %in% colnames(dt))) {
        p <- p +
          geom_ribbon(aes_string(
            ymin = sprintf('%s_LB', modelVarName),
            ymax = sprintf('%s_UB', modelVarName),
            fill = '"Min-max"'),
            alpha = 0.4
          ) +
          scale_fill_manual('Bounds', values = fillColor)
      }
    }
    if (!is.null(title)) {
      p <- p +
        ggtitle(title)
    }
    p <- p +
      scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
      scale_linetype_manual('Datasets', values = lineTypes) +
      GetPlotBase()
    return(p)
  }

  pTotalHIVInf <- ggplot(data = dt, aes(x = Year)) +
    scale_fill_manual('Bounds', values = colors[1:3]) +
    geom_line(aes(y = N_Alive, color = 'Alive'), size = 0.8) +
    geom_line(aes(y = N_Alive_Diag_M, color = 'Diagnosed'), size = 0.8) +
    geom_line(aes(y = N_Und, color = 'Undiagnosed'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual('Datasets', values = colors[1:3]) +
    GetPlotBase() +
    ggtitle('Total number of HIV-infected')
  if (all(c('N_Alive_LB', 'N_Alive_UB') %in% colnames(dt))) {
    pTotalHIVInf <- pTotalHIVInf +
      geom_ribbon(aes(ymin = N_Alive_LB, ymax = N_Alive_UB, fill = 'Alive Min-max'), alpha = 0.2)
  }
  if (all(c('N_Alive_Diag_M_LB', 'N_Alive_Diag_M_UB') %in% colnames(dt))) {
    pTotalHIVInf <- pTotalHIVInf +
      geom_ribbon(aes(ymin = N_Alive_Diag_M_LB, ymax = N_Alive_Diag_M_UB, fill = 'Diagnosed Min-max'),
                  alpha = 0.2)
  }
  if (all(c('N_Und_LB', 'N_Und_UB') %in% colnames(dt))) {
    pTotalHIVInf <- pTotalHIVInf +
      geom_ribbon(aes(ymin = N_Und_LB, ymax = N_Und_UB, fill = 'Undiagnosed Min-max'), alpha = 0.2)
  }

  plots <- list()
  plots[['HIV diagnoses, total']] <- GetPlot(dt, 'N_HIV_D', 'N_HIV_Obs_M', 'HIV diagnoses, total')
  plots[['HIV diagnoses, CD4 >= 500']] <- GetPlot(dt, 'N_CD4_1_D', 'N_CD4_1_Obs_M', 'HIV diagnoses, CD4 >= 500')
  plots[['HIV diagnoses, CD4 >= 350-499']] <- GetPlot(dt, 'N_CD4_2_D', 'N_CD4_2_Obs_M', 'HIV diagnoses, CD4 >= 350-499')
  plots[['HIV diagnoses, CD4 >= 200-349']] <- GetPlot(dt, 'N_CD4_3_D', 'N_CD4_3_Obs_M', 'HIV diagnoses, CD4 >= 200-349')
  plots[['HIV diagnoses, CD4 < 200']] <- GetPlot(dt, 'N_CD4_4_D', 'N_CD4_4_Obs_M', 'HIV diagnoses, CD4 < 200')
  plots[['HIV/AIDS diagnoses']] <- GetPlot(dt, 'N_HIVAIDS_D', 'N_HIVAIDS_Obs_M', 'HIV/AIDS diagnoses')
  plots[['AIDS diagnoses, total']] <- GetPlot(dt, 'N_AIDS_D', 'N_AIDS_M', 'AIDS diagnoses, total')
  plots[['HIV infections per year']] <- GetPlot(dt, NULL, 'N_Inf_M', 'HIV infections per year')
  plots[['Time to diagnosis, by year of infection']] <- GetPlot(dt, NULL, 't_diag', 'Time to diagnosis, by year of infection')
  plots[['Time to diagnosis, by year of diagnosis']] <- GetPlot(dt, NULL, 'D_Avg_Time', 'Time to diagnosis, by year of diagnosis')
  plots[['Total number of HIV-infected']] <- pTotalHIVInf
  plots[['Proportion undiagnosed of all those alive']] <- GetPlot(dt, NULL, 'N_Und_Alive_p', 'Proportion undiagnosed of all those alive')

  return(plots)
}
