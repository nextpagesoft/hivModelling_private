CreateOutputPlots <- function(
  mainResults,
  bsResultsList
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
  N_CD4_1_Obs_M_LB <- N_CD4_1_Obs_M_UB <- N_CD4_2_Obs_M_LB <- N_CD4_2_Obs_M_UB <-
    N_CD4_3_Obs_M_LB <- N_CD4_3_Obs_M_UB <- N_CD4_4_Obs_M_LB <- N_CD4_4_Obs_M_UB <- N_Dead_U <-
    N_HIV_Obs_M_LB <- N_HIV_Obs_M_UB <- Run <- NULL
  N_HIVAIDS_Obs_M_LB <- N_HIVAIDS_Obs_M_UB <- N_AIDS_M_LB <- N_AIDS_M_UB <- N_Inf_M_LB <-
    N_Inf_M_UB <- t_diag_LB <- t_diag_UB <- N_Alive_LB <- N_Alive_UB <- N_Alive_Diag_M_LB <-
    N_Alive_Diag_M_UB <- N_Und_LB <- N_Und_UB <- N_Und_Alive_p_LB <- N_Und_Alive_p_UB <- NULL

  colors <- c('#69b023', '#9d8b56', '#7bbcc0', '#ce80ce')
  fillColor <- '#d9d9d9'

  main <- mainResults$MainOutputs[, .(
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
    N_Alive, N_Alive_Diag_M, N_Und,
    N_Und_Alive_p
  )]
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
      N_Alive_LB, N_Alive_UB, N_Alive_Diag_M_LB, N_Alive_Diag_M_UB, N_Und_LB, N_Und_UB,
      N_Und_Alive_p_LB, N_Und_Alive_p_UB
    )
  ]
  dt <- merge(main, conf, by = 'Year', all = TRUE)

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

  plots <- list()

  plots[['HIV diagnoses, total']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_HIV_Obs_M_LB, ymax = N_HIV_Obs_M_UB, fill = 'Min-max'), alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_HIV_D, color = 'Data', linetype = 'Data')) +
    geom_line(aes(y = N_HIV_Obs_M, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    scale_linetype_manual('Datasets', values = c(Data = 'solid', Mean = 'dashed')) +
    GetPlotBase() +
    ggtitle('HIV diagnoses, total')

  plots[['HIV diagnoses, CD4 >= 500']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_CD4_1_Obs_M_LB, ymax = N_CD4_1_Obs_M_UB, fill = 'Min-max'),
                alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_CD4_1_D, color = 'Data', linetype = 'Data')) +
    geom_line(aes(y = N_CD4_1_Obs_M, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    scale_linetype_manual('Datasets', values = c(Data = 'solid', Mean = 'dashed')) +
    GetPlotBase() +
    ggtitle('HIV diagnoses, CD4 >= 500')

  plots[['HIV diagnoses, CD4 >= 350-499']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_CD4_2_Obs_M_LB, ymax = N_CD4_2_Obs_M_UB, fill = 'Min-max'),
                alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_CD4_2_D, color = 'Data', linetype = 'Data')) +
    geom_line(aes(y = N_CD4_2_Obs_M, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    scale_linetype_manual('Datasets', values = c(Data = 'solid', Mean = 'dashed')) +
    GetPlotBase() +
    ggtitle('HIV diagnoses, CD4 >= 350-499')

  plots[['HIV diagnoses, CD4 >= 200-349']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_CD4_3_Obs_M_LB, ymax = N_CD4_3_Obs_M_UB, fill = 'Min-max'),
                alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_CD4_3_D, color = 'Data', linetype = 'Data')) +
    geom_line(aes(y = N_CD4_3_Obs_M, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    scale_linetype_manual('Datasets', values = c(Data = 'solid', Mean = 'dashed')) +
    GetPlotBase() +
    ggtitle('HIV diagnoses, CD4 >= 200-349')

  plots[['HIV diagnoses, CD4 < 200']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_CD4_4_Obs_M_LB, ymax = N_CD4_4_Obs_M_UB, fill = 'Min-max'),
                alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_CD4_4_D, color = 'Data', linetype = 'Data')) +
    geom_line(aes(y = N_CD4_4_Obs_M, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    scale_linetype_manual('Datasets', values = c(Data = 'solid', Mean = 'dashed')) +
    GetPlotBase() +
    ggtitle('HIV diagnoses, CD4 >= 200-349')

  plots[['HIV/AIDS diagnoses']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_HIVAIDS_Obs_M_LB, ymax = N_HIVAIDS_Obs_M_UB, fill = 'Min-max'),
                alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_HIVAIDS_D, color = 'Data', linetype = 'Data')) +
    geom_line(aes(y = N_HIVAIDS_Obs_M, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    scale_linetype_manual('Datasets', values = c(Data = 'solid', Mean = 'dashed')) +
    GetPlotBase() +
    ggtitle('HIV/AIDS diagnoses')

  plots[['HIV diagnoses, total']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_AIDS_M_LB, ymax = N_AIDS_M_UB, fill = 'Min-max'), alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_AIDS_D, color = 'Data', linetype = 'Data')) +
    geom_line(aes(y = N_AIDS_M, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    scale_linetype_manual('Datasets', values = c(Data = 'solid', Mean = 'dashed')) +
    GetPlotBase() +
    ggtitle('HIV diagnoses, total')

  plots[['HIV infections per year']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_Inf_M_LB, ymax = N_Inf_M_UB, fill = 'Min-max'), alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_Inf_M, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    scale_linetype_manual('Datasets', values = c(Mean = 'solid')) +
    GetPlotBase() +
    ggtitle('HIV infections per year')

  plots[['Time to diagnosis']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = t_diag_LB, ymax = t_diag_UB, fill = 'Min-max'), alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = t_diag, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    GetPlotBase() +
    ggtitle('Time to diagnosis')

  plots[['Total number of HIV-infected']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_Alive_LB, ymax = N_Alive_UB, fill = 'Alive Min-max'), alpha = 0.2) +
    geom_ribbon(aes(ymin = N_Alive_Diag_M_LB, ymax = N_Alive_Diag_M_UB, fill = 'Diagnosed Min-max'),
                alpha = 0.2) +
    geom_ribbon(aes(ymin = N_Und_LB, ymax = N_Und_UB, fill = 'Undiagnosed Min-max'), alpha = 0.2) +
    scale_fill_manual('Bounds', values = colors[1:3]) +
    geom_line(aes(y = N_Alive, color = 'Alive'), size = 0.8) +
    geom_line(aes(y = N_Alive_Diag_M, color = 'Diagnosed'), size = 0.8) +
    geom_line(aes(y = N_Und, color = 'Undiagnosed'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual('Datasets', values = colors[1:3]) +
    GetPlotBase() +
    ggtitle('Total number of HIV-infected')

  plots[['Proportion undiagnosed of all those alive']] <-
    ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_Und_Alive_p_LB, ymax = N_Und_Alive_p_UB, fill = 'Min-max'),
                alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_Und_Alive_p, color = 'Mean', linetype = 'Mean'), size = 0.8) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[1], fillColor)) +
    scale_linetype_manual('Datasets', values = c(Mean = 'solid')) +
    GetPlotBase() +
    ggtitle('Proportion undiagnosed of all those alive')

  return(plots)
}
