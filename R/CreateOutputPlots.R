CreateOutputPlots <- function(
  mainResults,
  bsResultsList
) {
  main <- mainResults$MainOutputs[, .(Year, N_HIV_D, N_HIV_Obs_M)]
  conf <- bsResultsList$ConfBounds[Run == max(Run), .(Year, N_HIV_Obs_M_LB, N_HIV_Obs_M_UB)]
  dt <- merge(main, conf, by = 'Year', all = TRUE)

  colors <- c('#69b023', '#9d8b56', '#7bbcc0', '#ce80ce')
  fillColor <- '#d9d9d9'


  ggplot(data = dt, aes(x = Year)) +
    geom_ribbon(aes(ymin = N_HIV_Obs_M_LB, ymax = N_HIV_Obs_M_UB, fill = '95% confidence interval\nfor estimated total count'), alpha = 0.4) +
    scale_fill_manual('Bounds', values = fillColor) +
    geom_line(aes(y = N_HIV_D, color = 'Data', linetype = 'Data')) +
    geom_line(aes(y = N_HIV_Obs_M, color = 'Mean', linetype = 'Mean')) +
    labs(colour = 'Datasets', x = 'xxx', y = 'yyy') +
    scale_color_manual("Datasets", values = c(colors[3], colors[1])) +
    scale_linetype_manual('Datasets', values = c(Data = 'solid', Mean = 'dotted')) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic() +
    theme(plot.title = element_text(size = 12, face = 'plain'),
          text = element_text(size = 12, face = 'plain'),
          panel.grid = element_blank(),
          axis.line = element_line(colour = '#888888'),
          axis.ticks = element_line(colour = '#888888'),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11)) +
    ggtitle('HIV diagnoses, total') +
    xlab('Year') +
    ylab('Count')
}
