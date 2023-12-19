make_frames_AR <- function (proc_obj, recs = NULL, out_dir = getwd(), background_ylim = c(41.3, 
                                                                        49), background_xlim = c(-92.45, -75.87), show_interpolated = TRUE, 
          tail_dur = 0, animate = TRUE, ani_name = "animation.mp4", 
          frame_delete = FALSE, overwrite = FALSE, ffmpeg = NA, preview = FALSE, 
          bg_map = NULL, show_progress = TRUE, ...) 
{
  if (!missing(ffmpeg)) 
    warning("As of glatos v 0.4.1, make_frames() and ", 
            "make_video() no longer use ", "the external program ffmpeg. See ?make_video ", 
            " for details. ", "Input argument 'ffmpeg' has been ", 
            "ignored in this call and will be removed ", "in a future version of make_frames().", 
            call. = FALSE)
  if (animate) {
    starts_with_dot <- grepl(pattern = "^\\.", ani_name)
    if (!starts_with_dot) {
      contains_dir <- dirname(ani_name) != "."
      if (!contains_dir) 
        ani_name <- file.path(out_dir, ani_name)
    }
  }
  if (!overwrite & file.exists(ani_name)) 
    stop("Operation aborted ", "because output video file ", 
         "exists and 'overwrite = ", "FALSE'.", call. = FALSE)
  work_proc_obj <- data.table::as.data.table(proc_obj)
  work_proc_obj[, `:=`(row_in, 1:.N)]
  inargs <- list(...)
  rcv_args <- list(pch = 16, cex = 1.5)
  dtc_args <- list(pch = 16, col = "blue", cex = 2)
  par_inargs <- inargs[grepl("^par\\.", names(inargs))]
  rcv_inargs <- inargs[grepl("^recs\\.", names(inargs))]
  timeline_inargs <- inargs[grepl("^timeline\\.", names(inargs))]
  timeslider_inargs <- inargs[grepl("^timeslider\\.", names(inargs))]
  dtc_inarg_names <- setdiff(names(inargs), c(names(par_inargs), 
                                              names(rcv_inargs), names(timeline_inargs), names(timeslider_inargs)))
  dtc_inargs <- inargs[dtc_inarg_names]
  names(par_inargs) <- gsub("^par\\.", "", names(par_inargs))
  names(timeline_inargs) <- gsub("^timeline\\.", "", names(timeline_inargs))
  names(timeslider_inargs) <- gsub("^timeslider\\.", "", names(timeslider_inargs))
  names(rcv_inargs) <- gsub("^recs\\.", "", names(rcv_inargs))
  if (length(rcv_inargs) > 0) 
    rcv_args[names(rcv_inargs)] <- rcv_inargs
  if (length(dtc_inargs) > 0) 
    dtc_args[names(dtc_inargs)] <- dtc_inargs
  if (!is.null(recs)) {
    for (i in 1:length(rcv_args)) {
      if (length(rcv_args[[i]] == 1)) {
        rcv_args[[i]] <- rep(rcv_args[[i]], nrow(recs))
      }
      else if (length(rcv_args[[i]]) != nrow(recs)) {
        stop(paste0("Length of optional plot parameters pass via '...' ", 
                    "must be 1 or equal to\n number of rows in input data."))
      }
    }
  }
  for (i in 1:length(dtc_args)) {
    if (length(dtc_args[[i]]) == 1) {
      dtc_args[[i]] <- rep(dtc_args[[i]], nrow(work_proc_obj))
    }
    else if (length(dtc_args[[i]]) != nrow(work_proc_obj)) {
      stop(paste0("Length of optional plot parameters pass via '...' ", 
                  "must be 1 or equal to\n number of rows in input data."))
    }
  }
  rcv_args <- data.table::as.data.table(rcv_args)
  rcv_args[, `:=`(row_in, 1:.N)]
  dtc_args <- data.table::as.data.table(dtc_args)
  dtc_args[, `:=`(row_in, 1:.N)]
  if (!is.null(recs)) {
    recs <- data.table::as.data.table(recs)
    recs[, `:=`(row_in, 1:.N)]
    data.table::setkey(recs, recover_date_time)
    recs <- recs[!list(NA_real_), c("station", "deploy_lat", 
                                    "deploy_long", "deploy_date_time", "recover_date_time", 
                                    "row_in")]
  }
  if (!dir.exists(out_dir)) 
    dir.create(out_dir)
  t_seq <- unique(work_proc_obj$bin_timestamp)
  if (tail_dur == 0) {
    work_proc_obj[, `:=`(grp, bin_timestamp)]
  }
  else {
    dur <- work_proc_obj[, .(t_seq = sort(unique(bin_timestamp)))]
    dur[, `:=`(c("t_end", "t_grp"), list(data.table::shift(t_seq, 
                                                           type = "lag", fill = min(t_seq), n = tail_dur), 
                                         1:nrow(dur)))]
    work_proc_obj[, `:=`(t_end, bin_timestamp)]
    data.table::setkey(dur, t_end, t_seq)
    work_proc_obj <- data.table::foverlaps(work_proc_obj, 
                                           dur, type = "within", nomatch = 0L, by.x = c("bin_timestamp", 
                                                                                        "t_end"))
    work_proc_obj <- work_proc_obj[, c("animal_id", "t_seq", 
                                       "latitude", "longitude", "record_type", "row_in")]
    data.table::setnames(work_proc_obj, c("animal_id", "bin_timestamp", 
                                          "latitude", "longitude", "record_type", "row_in"))
    work_proc_obj[, `:=`(grp, bin_timestamp)]
  }
  data.table::setorder(work_proc_obj, bin_timestamp)
  work_proc_obj[, `:=`(grp_num, .GRP), by = bin_timestamp]
  char <- paste0("%", 0, nchar((length(t_seq))), "d")
  data.table::setkey(work_proc_obj, bin_timestamp)
  work_proc_obj[, `:=`(f_name, .GRP), by = grp]
  work_proc_obj[, `:=`(f_name, paste0(sprintf(char, f_name), 
                                      ".png"))]
  data.table::setkey(work_proc_obj, bin_timestamp, animal_id, 
                     record_type)
  if (is.null(bg_map)) {
    background <- greatLakesPoly
  }
  else {
    background <- bg_map
    if (is.null(background_ylim) | all(background_ylim == 
                                       c(41.3, 49))) 
      background_ylim <- as.numeric(bbox(bg_map)["y", 
      ])
    if (is.null(background_xlim) | all(background_xlim == 
                                       c(-92.45, -75.87))) 
      background_xlim <- as.numeric(bbox(bg_map)["x", 
      ])
  }
  if (!show_interpolated) {
    work_proc_obj[record_type == "interpolated", `:=`(latitude, 
                                                      NA)]
    work_proc_obj[record_type == "interpolated", `:=`(longitude, 
                                                      NA)]
  }
  time_period <- range(work_proc_obj$bin_timestamp)
  cust_plot <- function(x, .time_period, .recs, .out_dir, 
                        .background, .background_xlim, .background_ylim) {
    linear_x = geosphere::distMeeus(c(.background_xlim[1], 
                                      .background_ylim[1]), c(.background_xlim[2], .background_ylim[1]))
    linear_y = geosphere::distMeeus(c(.background_xlim[1], 
                                      .background_ylim[1]), c(.background_xlim[1], .background_ylim[2]))
    figRatio <- linear_y/linear_x
    height <- trunc(2000 * figRatio)
    png(file.path(.out_dir, x$f_name[1]), width = 2000, 
        height = ifelse(height%%2 == 0, height, height + 
                          1), units = "px", pointsize = 22 * figRatio)
    par_args <- list(oma = c(0, 0, 0, 0), mar = c(6, 0, 
                                                  0, 0), xpd = FALSE)
    if (length(par_inargs) > 0) 
      par_args[names(par_inargs)] <- par_inargs
    do.call(par, par_args)
    sp::plot(.background, ylim = c(.background_ylim), xlim = c(.background_xlim), 
             axes = FALSE, lwd = 2 * figRatio, col = "white", 
             bg = "gray74")
    box(lwd = 3 * figRatio)
    if (!is.null(.recs)) {
      sub_recs <- .recs[deploy_date_time <= x$bin_timestamp[1] & 
                          (recover_date_time >= x$bin_timestamp[1] & !is.na(recover_date_time))]
      sub_rcv_args <- rcv_args[match(sub_recs$row_in, 
                                     rcv_args$row_in), ]
      do.call(points, c(list(x = sub_recs$deploy_long, 
                             y = sub_recs$deploy_lat), sub_rcv_args[, !"row_in", 
                                                                    with = FALSE]))
    }
    par(xpd = TRUE)
    xlim_diff <- diff(.background_xlim)
    ylim_diff <- diff(.background_ylim)
    timeline_y <- rep(.background_ylim[1] - (0.06 * ylim_diff), 
                      2)
    timeline_x <- c(.background_xlim[1] + (0.1 * xlim_diff), 
                    .background_xlim[2] - (0.1 * xlim_diff))
    time_dur <- diff(as.numeric(.time_period))
    labels <- seq(as.POSIXct(format(min(.time_period), "%Y-%m-%d")), 
                  as.POSIXct(format(max(.time_period), "%Y-%m-%d")), 
                  length.out = 5)
    labels_ticks <- as.POSIXct(format(labels, "%Y-%m-%d"), 
                               tz = "GMT")
    ptime <- (as.numeric(labels_ticks) - as.numeric(min(.time_period)))/time_dur
    labels_x <- timeline_x[1] + (diff(timeline_x) * ptime)
    timeline_args = list(side = 1, at = labels_x, pos = timeline_y[1], 
                         labels = format(labels, "%Y-%m-%d"), col = "grey70", 
                         lwd = 20 * figRatio, lend = 0, lwd.ticks = NA, col.ticks = 1, 
                         cex.axis = 2, padj = 0.5)
    if (length(timeline_inargs) > 0) 
      timeline_args[names(timeline_inargs)] <- timeline_inargs
    do.call(axis, timeline_args)
    ptime <- (as.numeric(x[1, "grp"]) - as.numeric(min(.time_period)))/time_dur
    timeline_x_i <- timeline_x[1] + diff(timeline_x) * ptime
    timeslider_args = list(pch = 21, cex = 2, bg = "grey40", 
                           col = "grey20", lwd = 1)
    if (length(timeslider_inargs) > 0) 
      timeslider_args[names(timeslider_inargs)] <- timeslider_inargs
    do.call(points, c(list(x = timeline_x_i, y = timeline_args$pos), 
                      timeslider_args))
    sub_dtc_args <- dtc_args[match(x$row_in, dtc_args$row_in), 
    ]
    do.call(points, c(list(x = x$longitude, y = x$latitude), 
                      sub_dtc_args[, !"row_in", with = FALSE]))
    # New code to add week label in the upper right section
    week_label <- format(x$bin_timestamp[1], "%Y-%m-%d")
    text_label_args <- list(x = timeline_x[2] - (0.75 * xlim_diff), 
                            y = timeline_y[1] + (0.95 * ylim_diff),
                            label = week_label, 
                            col = "black", cex = 10 * figRatio)
    do.call(text, text_label_args)
    dev.off()
  }
  data.table::setkey(work_proc_obj, grp_num)
  if (preview) {
    grpn <- 1
  }
  else {
    grpn <- data.table::uniqueN(work_proc_obj$grp)
    if (show_progress) 
      pb <- txtProgressBar(min = 0, max = grpn, style = 3)
  }
  work_proc_obj[grp_num <= grpn, {
    if (!preview & show_progress) 
      setTxtProgressBar(pb, .GRP)
    cust_plot(x = .SD, .time_period = time_period, .recs = recs, 
              .out_dir = out_dir, .background = background, .background_xlim = background_xlim, 
              .background_ylim = background_ylim)
  }, by = grp, .SDcols = c("bin_timestamp", "longitude", "latitude", 
                           "record_type", "f_name", "grp", "row_in")]
  if (preview) {
    message("Preview frames written to\n ", out_dir)
  }
  else {
    if (show_progress) 
      close(pb)
    if (animate) {
      make_video(input_dir = out_dir, output = ani_name, 
                 overwrite = overwrite)
    }
    if (frame_delete) {
      unlink(file.path(out_dir, unique(work_proc_obj$f_name)))
    }
    else {
      message("Frames written to\n ", out_dir)
    }
  }
}
