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

  plot_data = tidyr::gather(ind_data, key = 'indicator', value = 'valor', DIST:EFF_VIAJE)

  png(file.path(save_folder, 'indicators_plot.png'), width = 180, height = 180, units = 'mm', res = 300, ...)

  print(ggplot2::ggplot(data = plot_data, aes(x = TIME_IND, y = valor)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab('Periodo') +
    ggplot2::ylab('Valor') +
    ggplot2::facet_wrap(vars(indicator), nrow = 3, scales = "free_y"))

  dev.off()

}
