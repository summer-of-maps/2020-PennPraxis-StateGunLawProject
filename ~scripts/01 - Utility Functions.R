##########################################################################
# This script:
# 1. Defines utility functions for use in the analysis.
#
# Exports:
# 1. base_map_bb(): Get a bounding box for use with the ggmap package's get_map() function
# 2. replace_lat_lon_cols(): some cities in the guns dataset have lon/lat in projected coordinates.
#     This replaces them with coordinates in a new crs (default WGS84)
# 3. intersect_geo_lists(): loop of two lists of sf objects (list1 and list2) to keep the 
#     features in each list1 element that intersect with a feature in the corresponding 
#     list2 element.
# 4. 
#   a. find_Moran_I(): input sf polygons with var of interest and return output of moran.test()
#     for those polygons
#   b. find_localMoran(): input sf polygons with var of interest and return output of localmoran()
#     for those polygons
# 5. q5(): factor continuous variable in quintiles
# 6. qBr(): find quintile breaks
# 7. %!in%: negate %in%
# 8. align_plots_diffAxes() and plot_grid_diffAxes(): modifies native `cowplot` functions to 
#     allow alignment of grobs by x- or y-axis with different limits. See:
#     https://stackoverflow.com/questions/57392541/extend-axis-limits-without-plotting-in-order-to-align-two-plots-by-x-unit
# 9. rename_census_cols(): rename census variable ID columns with a vector of names and 
#   optionally drop the margin of error columns
#
# To-do:
# 1. 
##########################################################################

## 1. ----
base_map_bb <- function(sf # this should be an sf object
                        ) {
  tmaptools::bb(sf, output = "matrix")
}

## 2. ----
replace_lat_lon_cols <- function(data, # e.g., df with lon/lat columns guns_list$Dallas
                                 current_crs, # the current proj for the columns
                                 new_crs = 4326 # the desired proj
) {
  
  tmp_col_names <- names(data)
  data$tmp_ID <- 1:nrow(data)
  
  tmp <- data %>%
    filter(!is.na(lon),
           !is.na(lat)) %>% 
    # make sf object using raw coordinates, which are in 2276 projection
    st_as_sf(.,
             # lon/lat are flipped in the raw data. "lat" column contains the longitude coordinates.
             coords = c("lon", "lat"),
             crs = current_crs,
             remove = FALSE) %>% 
    # reproject to WGS84
    st_transform(crs = new_crs)
  
  tmp_coords <- st_coordinates(tmp) # get coordinates of tmp object
  
  tmp <- tmp %>% 
    # replace lon/lat column of tmp objects with the coordinates from tmp_coords
    mutate(lon = tmp_coords[, 1],
           lat = tmp_coords[, 2]) %>% 
    dplyr::select(lon, lat, tmp_ID) %>% 
    st_drop_geometry()
  
  data %>% 
    # merge new WGS84 coordinates back into Dallas dataframe
    dplyr::select(-c(lon, lat)) %>% 
    left_join(tmp, by = "tmp_ID") %>% 
    # reset the order of the columns and drop the tmp_ID column
    dplyr::select(all_of(tmp_col_names))
}

## 3. ----
intersect_geo_lists <- function(list1, # features you want to filter
                                list2  # features to filter by
                                ) {
  
  suppressMessages(
  
    map2(list1,
       list2,
       ~ .x[.y,])
  
    )
}

## 4a. ----
find_Moran_I <- function(shp, # geographies of interest
                         var_name = "gun_count",
                         queen = TRUE, # neighbor type 
                         style = "W" # neighbor weight style - see ?nb2listw()
) {
  
  ## Returns the output of function moran.test()
  
  # make sp object
  sp <- as(shp, "Spatial")
  # find neighbors
  neighb <- poly2nb(sp, 
                    queen = queen)
  # find neighbor weights
  weights <- nb2listw(neighb,
                      style = style,
                      zero.policy = TRUE)
  
  # calculate Moran's I
  moran.test(sp[[var_name]],
             weights,
             zero.policy = TRUE)
  
}

## 4b. ----
find_localMoran <- function(shp, # geographies of interest
                            var_name = "gun_count",
                            queen = TRUE, # neighbor type 
                            style = "W" # neighbor weight style - see ?nb2listw()
) {
  
  ## Returns the output of function moran.test()
  
  # make sp object
  sp <- as(shp, "Spatial")
  # find neighbors
  neighb <- poly2nb(sp, 
                    queen = queen)
  # find neighbor weights
  weights <- nb2listw(neighb,
                      style = style,
                      zero.policy = TRUE)
  
  # calculate Moran's I
  localmoran(sp[[var_name]],
             weights,
             zero.policy = TRUE)
}

## 5. ----
q5 <- function(variable) {as.factor(ntile(variable, 5))}

## 6. ----
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

## 7. ----
'%!in%' <- function(x,y)!('%in%'(x,y))

## 8. ----
# https://stackoverflow.com/a/57575725/12211397
align_plots_diffAxes <- function (..., plotlist = NULL, align = c("none", "h", "v", "hv"),
                                  axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"), 
                                  greedy = TRUE) {
  plots <- c(list(...), plotlist)
  num_plots <- length(plots)
  grobs <- lapply(plots, function(x) {
    if (!is.null(x)) as_gtable(x)
    else NULL
  })
  halign <- switch(align[1], h = TRUE, vh = TRUE, hv = TRUE, FALSE)
  valign <- switch(align[1], v = TRUE, vh = TRUE, hv = TRUE, FALSE)
  vcomplex_align <- hcomplex_align <- FALSE
  if (valign) {
    
    # modification: get x-axis value range associated with each plot, create union of
    # value ranges across all plots, & calculate the proportional width of each plot
    # (with white space on either side) required in order for the plots to align
    plot.x.range <- lapply(plots, function(x) ggplot_build(x)$layout$panel_params[[1]]$x.range)
    full.range <- range(plot.x.range)
    plot.x.range <- lapply(plot.x.range,
                           function(x) c(diff(c(full.range[1], x[1]))/ diff(full.range),
                                         diff(x)/ diff(full.range),
                                         diff(c(x[2], full.range[2]))/ diff(full.range)))
    
    num_widths <- unique(lapply(grobs, function(x) {
      length(x$widths)
    }))
    num_widths[num_widths == 0] <- NULL
    if (length(num_widths) > 1 || length(grep("l|r", axis[1])) > 0) {
      vcomplex_align = TRUE
      warning("Method not implemented for faceted plots. Placing unaligned.")
      valign <- FALSE
    }
    else {
      max_widths <- list(do.call(grid::unit.pmax, 
                                 lapply(grobs, function(x) {x$widths})))
    }
  }
  if (halign) {
    
    # modification: get y-axis value range associated with each plot, create union of
    # value ranges across all plots, & calculate the proportional width of each plot
    # (with white space on either side) required in order for the plots to align
    plot.y.range <- lapply(plots, function(x) ggplot_build(x)$layout$panel_params[[1]]$y.range)
    full.range <- range(plot.y.range)
    plot.y.range <- lapply(plot.y.range,
                           function(x) c(diff(c(full.range[1], x[1]))/ diff(full.range),
                                         diff(x)/ diff(full.range),
                                         diff(c(x[2], full.range[2]))/ diff(full.range)))
    
    num_heights <- unique(lapply(grobs, function(x) {
      length(x$heights)
    }))
    num_heights[num_heights == 0] <- NULL
    if (length(num_heights) > 1 || length(grep("t|b", axis[1])) > 0) {
      hcomplex_align = TRUE
      warning("Method not implemented for faceted plots. Placing unaligned.")
      halign <- FALSE
    }
    else {
      max_heights <- list(do.call(grid::unit.pmax, 
                                  lapply(grobs, function(x) {x$heights})))
    }
  }
  for (i in 1:num_plots) {
    if (!is.null(grobs[[i]])) {
      if (valign) {
        grobs[[i]]$widths <- max_widths[[1]]
        
        # modification: change panel cell's width to a proportion of unit(1, "null"),
        # then add whitespace to the left / right of the plot's existing gtable
        grobs[[i]]$widths[[5]] <- unit(plot.x.range[[i]][2], "null")
        grobs[[i]] <- gtable::gtable_add_cols(grobs[[i]], 
                                              widths = unit(plot.x.range[[i]][1], "null"), 
                                              pos = 0)
        grobs[[i]] <- gtable::gtable_add_cols(grobs[[i]], 
                                              widths = unit(plot.x.range[[i]][3], "null"), 
                                              pos = -1)
      }
      if (halign) {
        grobs[[i]]$heights <- max_heights[[1]]
        
        # modification: change panel cell's height to a proportion of unit(1, "null"),
        # then add whitespace to the bottom / top of the plot's existing gtable
        grobs[[i]]$heights[[7]] <- unit(plot.y.range[[i]][2], "null")
        grobs[[i]] <- gtable::gtable_add_rows(grobs[[i]], 
                                              heights = unit(plot.y.range[[i]][1], "null"), 
                                              pos = -1)
        grobs[[i]] <- gtable::gtable_add_rows(grobs[[i]], 
                                              heights = unit(plot.y.range[[i]][3], "null"), 
                                              pos = 0)
      }
    }
  }
  grobs
}

plot_grid_diffAxes <- function (..., plotlist = NULL, align = c("none", "h", "v", 
                                                                "hv"), axis = c("none", "l", "r", "t", "b", "lr", "tb", 
                                                                                "tblr"), nrow = NULL, ncol = NULL, rel_widths = 1, rel_heights = 1, 
                                labels = NULL, label_size = 14, label_fontfamily = NULL, 
                                label_fontface = "bold", label_colour = NULL, label_x = 0, 
                                label_y = 1, hjust = -0.5, vjust = 1.5, scale = 1, greedy = TRUE, 
                                cols = NULL, rows = NULL) 
{
  plots <- c(list(...), plotlist)
  num_plots <- length(plots)
  if (!is.null(cols)) {
    warning("Argument 'cols' is deprecated. Use 'ncol' instead.")
  }
  if (!is.null(rows)) {
    warning("Argument 'rows' is deprecated. Use 'nrow' instead.")
  }
  scale <- rep_len(scale, num_plots)
  if (sum(scale <= 0) > 1) {
    stop("Argument 'scale' needs to be greater than 0.")
  }
  if (!is.null(ncol)) {
    cols <- ncol
  }
  if (!is.null(nrow)) {
    rows <- nrow
  }
  grobs <- align_plots_diffAxes(plotlist = plots, align = align, axis = axis, 
                                greedy = greedy)
  if (is.null(cols) && is.null(rows)) {
    cols <- ceiling(sqrt(num_plots))
    rows <- ceiling(num_plots/cols)
  }
  if (is.null(cols)) 
    cols <- ceiling(num_plots/rows)
  if (is.null(rows)) 
    rows <- ceiling(num_plots/cols)
  if ("AUTO" %in% labels) 
    labels <- LETTERS[1:num_plots]
  else if ("auto" %in% labels) 
    labels <- letters[1:num_plots]
  hjust <- rep_len(hjust, length(labels))
  vjust <- rep_len(vjust, length(labels))
  label_x <- rep_len(label_x, length(labels))
  label_y <- rep_len(label_y, length(labels))
  rel_heights <- rep(rel_heights, length.out = rows)
  rel_widths <- rep(rel_widths, length.out = cols)
  x_deltas <- rel_widths/sum(rel_widths)
  y_deltas <- rel_heights/sum(rel_heights)
  xs <- cumsum(rel_widths)/sum(rel_widths) - x_deltas
  ys <- 1 - cumsum(rel_heights)/sum(rel_heights)
  p <- ggdraw()
  col_count <- 0
  row_count <- 1
  for (i in 1:(rows * cols)) {
    if (i > num_plots) 
      break
    x_delta <- x_deltas[col_count + 1]
    y_delta <- y_deltas[row_count]
    x <- xs[col_count + 1]
    y <- ys[row_count]
    p_next <- grobs[[i]]
    if (!is.null(p_next)) {
      p <- p + draw_grob(p_next, x, y, x_delta, y_delta, 
                         scale[i])
    }
    if (i <= length(labels)) {
      p <- p + draw_plot_label(labels[i], x + label_x[i] * 
                                 x_delta, y + label_y[i] * y_delta, size = label_size, 
                               family = label_fontfamily, fontface = label_fontface, 
                               colour = label_colour, hjust = hjust[i], vjust = vjust[i])
    }
    col_count <- col_count + 1
    if (col_count >= cols) {
      col_count <- 0
      row_count <- row_count + 1
    }
  }
  p
}

## 9. ----
rename_census_cols <- function(x,
                               vars,
                               names,
                               drop_MOE = TRUE # drop margin of error?
){
  
  estimate <- paste(vars, "E", sep = "")
  MOE <- paste(vars, "M", sep = "")
  
  if(drop_MOE == TRUE | drop_MOE == T) {
    
    output <- x %>% 
      rename_at(vars(estimate), 
                ~ names) %>% 
      dplyr::select(-all_of(MOE))
    
  } else if (missing(drop_MOE) | drop_MOE == FALSE | drop_MOE == F) {
    
    output <- x %>% 
      rename_at(vars(estimate), 
                ~ names)
    
  }
  
  output
}
