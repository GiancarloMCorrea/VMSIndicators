#' Plot efficiency indicators
#'
#' Make a plot of the nine efficiency indicators per period (YYYYMM).
#'
#' @param ind_data Dataset with efficiency indicators obtained from the
#' get_indicators function.
#' @param save_folder Folder to save the plot.
#' @return A PNG with the efficiency indicators plot.
#' @export
plot_indicators = function(ind_data, save_folder = './', ...) {

  plot_data = gather(ind_data, key = 'indicator', value = 'valor', IND_1:IND_9)

  png(file.path(save_folder, 'indicators_plot.png'), width = 180, height = 180, units = 'mm', res = 300, ...)

  print(ggplot(data = plot_data, aes(x = TIME_IND, y = valor)) +
    geom_boxplot() +
    xlab('Periodo') +
    ylab('Valor') +
    facet_wrap(vars(indicator), nrow = 3, scales = "free_y"))

  dev.off()

}
