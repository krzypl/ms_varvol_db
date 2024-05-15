mgcv2tapas_krzypl2 <- function (series = NULL, data_type = "accI", series.name = NA, 
                        plotit = TRUE) 
{
  x_var_name <- series[["pred.formula"]][[2]]
  y_var_name <- series[["pterms"]][[2]]
  age_top <- series[["model"]] %>% dplyr::pull(dplyr::all_of(x_var_name))
  y_raw <- series[["model"]] %>% dplyr::pull(dplyr::all_of(y_var_name))
  age_bot <- age_top - 1
  volI <- 1
  if (sd(diff(age_top)) == 0) {
    yr.interp <- mean(diff(age_top))
    series_type <- "binned"
  }
  else {
    yr.interp <- NULL
    series_type <- "not-binned"
  }
  y_trend <- mgcv::predict.gam(object = series, se.fit = T, 
                               type = "response")$fit
  y_se <- mgcv::predict.gam(object = series, se.fit = T, type = "response")$se.fit
  y_detr <- y_raw - y_trend
  if (data_type == "accI" | data_type == "acc") {
    y_conc <- y_raw * abs(age_top - age_bot)
    y_counts <- y_conc * volI
    y_name <- "varve_thick"
  }
  if (data_type == "conI" | data_type == "con") {
    y_conc <- y_raw
    y_counts <- y_raw * volI
    y_name <- y_var_name
  }
  if (data_type == "countI" | data_type == "count") {
    y_conc <- y_raw/volI
    y_counts <- y_raw
    y_name <- y_var_name
  }
  series <- as.data.frame(cbind(age_top, y_raw))
  colnames(series) <- c("age", y_var_name)
  raw <- list(series = series, series.name = series.name)
  series.int <- series
  series.conI <- as.data.frame(cbind(age_top, y_conc))
  colnames(series.conI) <- c("age", y_name)
  series.countI <- as.data.frame(cbind(age_top, y_counts))
  colnames(series.countI) <- c("age", y_name)
  int <- list(series.int = series.int, series.conI = series.conI, 
              series.countI = series.countI, volI = volI, yr.interp = yr.interp, 
              type = series_type)
  detr <- as.data.frame(cbind(age_top, y_detr))
  colnames(detr) <- c("age", y_var_name)
  detr <- list(detr = detr, smoothing.yr = NULL, detr.type = "GAM", 
               series.name = series.name)
  output <- list(raw = raw, int = int, out = data_type, detr = detr)
  if (plotit == T) {
    x_lim <- c(max(age_top), min(age_top))
    y_lim <- c(0, max(y_raw))
    par(mfrow = c(2, 1), mar = c(2, 5, 2, 2), oma = c(2, 
                                                      1, 1, 1))
    plot(age_top, y_raw, type = "s", xlim = x_lim, ylim = y_lim, 
         xlab = x_var_name, ylab = y_var_name, axes = F)
    axis(1)
    axis(2)
    lines(age_top, y_trend, col = "red")
    lines(age_top, y_trend + 2 * y_se, col = "blue")
    lines(age_top, y_trend - 2 * y_se, col = "blue")
    x_lim <- c(max(age_top), min(age_top))
    y_lim <- c(min(y_detr), max(y_detr))
    plot(age_top, y_detr, type = "s", xlim = x_lim, ylim = y_lim, 
         xlab = x_var_name, ylab = paste("detrended", y_var_name), 
         axes = F)
    axis(1)
    axis(2)
    abline(h = 0)
    lines(age_top, 2 * y_se, col = "red", lwd = 1)
    lines(age_top, -2 * y_se, col = "red", lwd = 1)
  }
  return(output)
}
