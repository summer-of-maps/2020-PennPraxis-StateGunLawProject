##########################################################################
# This script:
# 1. Splits the data into even and odd years for each city
#     Samples one year's word of data for each of those for the Multi-Scale Error Analysis
# 2. Define the MSEA_EC() function
#     Reference: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0218324
# 3. Defines parameters for the msea function
# 4. Loops over the cities, saving interim objects as it proceeds
#
# Exports: 
# 1. guns_list_shp_evenYears as 24_guns_list_shp_evenYears.rds
# 2. guns_list_shp_oddYears as 24_guns_list_shp_oddYears.rds
# 
# To-do:
# 1. 
#
##########################################################################

# 1. Split into even and odd years ----
# For every city, split into odd years and even years
# Get a sample of the crimes that is equal to the average number of crimes that 
# occurs in one of those even/odd years

# years_byCity <- readRDS("~outputs/20/23_years_byCity.rds")
years_tmp <- map(years_byCity,
                 ~ names(.x) %>% as.numeric)
evenYears_tmp <- map2(years_byCity,
                  years_tmp,
                  ~ .x[.y %% 2 == 0])
oddYears_tmp <- map2(years_byCity,
                  years_tmp,
                  ~ .x[.y %% 2 != 0])

# guns_list_shp_evenYears <- readRDS("~outputs/20/24_guns_list_shp_evenYears.rds")
guns_list_shp_evenYears <- map2(
  guns_list_shp,
  evenYears_tmp,
  ~ filter(.x,
           year %in% .y) %>% 
    # sample an average of one year's worth of those crimes
    slice_sample(prop = 1 / length(.y))
  )

# guns_list_shp_oddYears <- readRDS("~outputs/20/24_guns_list_shp_oddYears.rds")
guns_list_shp_oddYears <- map2(
  guns_list_shp,
  oddYears_tmp,
  ~ filter(.x,
           year %in% .y) %>% 
    # sample an average of one year's worth of those crimes
    slice_sample(prop = 1 / length(.y))
)

## 2. ----
msea_EC <- function(points1, points2, N = 20, n.shifts = 10, mask, ignore.zeros = FALSE, step = 1, 
                    return.sobject = TRUE, return.grids = TRUE,
                    sppt_func = sppt, ... ) {
  
  points1_sp <- as(points1, "Spatial")
  points2_sp <- as(points2, "Spatial")
  
  points_all <- rbind(points1, points2)
  
  bb <- st_bbox(points_all) # A bounding box around all points
  
  # Run each resolution in parallel. The output is a named list with all of the different elements in it
  # resolutions is a list of all resolutions to run at
  run.resolution <- function(resolutions) {
    
    print(paste("Resolution:", resolutions, "of", N))
    
    output <- list()
    
    # Store all grids (data frames) in a big long list
    output[["results"]] <- list()
    
    # Remember some other things that are useful later
    output[["cell.areas"]] <- c() # The area of the cells
    output[["num.cells"]] <- c()  # The number of cells in each iteration
    output[["n.parameter"]] <- c()  # The n parameter from st_make_grid()
    
    # Remember the global errors associated with each grid
    output[["rss"]] <- c() # Residual sum of squares
    output[["r.squared"]] <- c()
    output[["rmse"]] <- c()
    output[["globalS"]] <- c()
    output[["globalS.robust"]] <- c()
    output[["mean.similarity"]] <- c()
    output[["mean.similarity.robust"]] <- c()
    
    # Keep a link to the object that is returned from the call to sppt. Useful for debugging mostly.
    output[["s.object"]] <- list()
    output[["grid"]] <- list()
    
    # Remember the iteration and shift numbers (these are the i and j in the nested loops)
    output[["iteration"]] <- c()
    output[["shift"]] <- c()
    
    # Create the grids - adapted from Brunsdon & Comber (2015, p150)
    # Note: will actually start at i=2 which gives 2*2=4 cells (doesn't make sense to calculate error for 1 data point)
    # but it's easier to start from i=1 and delete that result afterwards
    for (i in resolutions) {
      # Cell size is the total width divided by the number of cells to draw so far (i)
      cell.width <-  unname((bb[3] - bb[1]) / i)
      cell.height <- unname((bb[4] - bb[2]) / i)
      
      # Make the bounding box slightly larger than necessary (by half a cell in each direction), 
      # so when the grid is shifted there wont be any points outside
      # It needs to be big enough so that it can have one extra ring of cells around it
      bb.larger <- bb # The new bounding box
      #bb.larger["coords.x1","min"] <- bb["coords.x1","min"] - ( cell.width / 2  ) # Min x gets smaller
      #bb.larger["coords.x2","min"] <- bb["coords.x2","min"] - ( cell.height / 2 ) # Min y gets smaller
      #bb.larger["coords.x1","max"] <- bb["coords.x1","max"] + ( cell.width / 2  ) # Max x gets larger
      #bb.larger["coords.x2","max"] <- bb["coords.x2","max"] + ( cell.height / 2 ) # Max y gets larger
      bb.larger[1] <- bb[1] - ( cell.width / 2  ) # Min x gets smaller
      bb.larger[2] <- bb[2] - ( cell.height / 2 ) # Min y gets smaller
      bb.larger[3] <- bb[3] + ( cell.width / 2  ) # Max x gets larger
      bb.larger[4] <- bb[4] + ( cell.height / 2 ) # Max y gets larger
      
      # For each resolution, repeat a few times by slightly shifting the grid by a random amount in a random direction
      for (j in 1:n.shifts) {
        print(paste("Shift:", j, "of", n.shifts))
        # Remember the cell area (useful later) (needs to be repeated for each shift)
        output[["cell.areas"]] <- c(output[["cell.areas"]], (cell.width * cell.height))
        
        # Chose random N-S and E-W directions to shift the grid in (using a random uniform distribution)
        shift.x <- runif(n = 1, min = -cell.width / 2, max = cell.width / 2)
        shift.y <- runif(n = 1, min = -cell.height / 2, max = cell.height / 2)
        
        # Calculate the centre of the lower-left cell (the one with the smallest coordinates),
        # taking into account the shift
        centre.x <- bb.larger[1] + ( cell.width  / 2 ) + shift.x
        centre.y <- bb.larger[2] + ( cell.height / 2 ) + shift.y
        
        n.parameter <- i+1
        
        output[["n.parameter"]] <- c(output[["n.parameter"]], n.parameter ** 2) 
        
        # Create a grid  
        grd <- suppressMessages(st_make_grid(mask,
          offset = c(centre.x, centre.y), # No offset, the grid will just cover all the points
          # cellsize = c(cell.width, cell.height), # don't include this. makes cells have weird sizes.
          n = c(n.parameter,n.parameter)
        ))
        
        # Convert the grid into an sf
        spdf <- st_sf(grd,
                      crs = st_crs(points1)) %>% 
          mutate(CellID = 1:nrow(.))
        

        # Remove any cells that don't intersect the mask?
          #browser()
          # Join spdf to the mask. Returns rows for all spdf, which will be NA if there was no join
          a <- suppressMessages(spdf[mask,])
          # Check some cells overlap, otherwise something has almost certainly gone wrong. (Doesn't matter
          # which field we check so just do first; a[1])
          stopifnot(length(which(!is.na(a[1]))  ) > 0 )
          # Remove those that have NA for the first column (could have chosen any)
          spdf <- spdf[which(!is.na(a[1])),]
          # rm(a)
          
          number.of.cells <- nrow(spdf) # Add an extra row and column to account for shifting 
          # Remember the number of cells in this iteration:
          output[["num.cells"]] <- c(output[["num.cells"]], number.of.cells) 
        
        # Aggregate the points
        spdf <- spdf %>% 
          {suppressMessages(mutate(.,
                                   points1_cnt = lengths(st_intersects(., points1)),
                                   points2_cnt = lengths(st_intersects(., points2))))}
        
        # store the grid cells (before aggregating points or filtering)
        output[["grid"]] <- append(output[["grid"]], list(spdf))
        
        # Drop cells with 0 for both counts?
        if (ignore.zeros) {
          spdf <- spdf[which(spdf$points1_cnt > 0 | spdf$points2_cnt > 0),]
          stopifnot(length(which(spdf$points1_cnt == 0 & spdf$points2_cnt == 0)) == 0 )
        }
        
        # Calculate percentages of points in each area (might be useful)
        spdf$p1.pct <- 100 * spdf$points1_cnt / sum(spdf$points1_cnt)
        spdf$p2.pct <- 100 * spdf$points2_cnt / sum(spdf$points2_cnt)
        
        # Calculate the errors 
        
        # Difference in the number of points
        spdf$diff <- spdf$points1_cnt - spdf$points2_cnt
        
        # Absolute difference
        spdf$abs.diff <- abs(spdf$points1_cnt - spdf$points2_cnt)
        
        # Absolute Difference in percentages
        spdf$abs.pct.diff <- abs(spdf$p1.pct - spdf$p2.pct)
        
        # The Local S Index (slightly more convoluted)
        spdf_sp <- as(spdf, "Spatial")
        
        s <- sppt_func(points1_sp, points2_sp, spdf_sp, ... ) %>%  # Calculate the index
          st_as_sf()
        
        # Sanity check - check the sppt package calculates the same percentages as this code
        stopifnot( identical(s$CellID, spdf$CellID) ) # Check the cells are in the same order (avoids having to merge on cell ID)
        
        # Useful Stats. associated with the S Index 
        # (the whole S object is also returned later too, but these are more convenient to have direct access to)
        spdf$localS            <- s$localS
        spdf$localS.robust     <- s$localS.robust
        spdf$similarity.robust <- s$similarity.robust
        spdf$ConfLowP          <- s$ConfLowP
        spdf$ConfUppP          <- s$ConfUppP
        spdf$similarity        <- s$similarity
        spdf$similarity.robust <- s$similarity.robust
        
        # Optionally store this result by appending it to the end of the list of results that we have so far
        if (return.grids) {
          output[["results"]][[ length(output[["results"]]) + 1 ]] <- spdf
        } else {
          output[["results"]][[ length(output[["results"]]) + 1 ]] <- NA
        }
        
        # Now calculate the global errors
        
        # RSS 
        output[["rss"]] <- c(output[["rss"]], sum((spdf$points1_cnt - spdf$points2_cnt) ** 2))
        # R squared
        output[["r.squared"]] <- c(output[["r.squared"]], 
                                   summary(lm(spdf$points1_cnt ~ spdf$points2_cnt, data = spdf))$r.squared )
        # RMSE
        output[["rmse"]] <- c(output[["rmse"]], rmse(spdf$points1_cnt, spdf$points2_cnt))
        
        # Global S Index (normal and robust). In globalS, each area has same value for global S, so take 1st row
        # arbitrarily. For robust version, need to find the first row that isn't NA (hence use min()).
        output[["globalS"]] <- c(output[["globalS"]], st_drop_geometry(s[1,"globalS"])) 
        output[["globalS.robust"]] <- c(output[["globalS.robust"]],
                                        st_drop_geometry(s[min(which(!is.na(s$globalS.robust))),"globalS.robust"]))
        
        # Mean Similarity. Areas are either similar (1) or dissimilar (0). Take the mean.
        # This is only possible using the newer sppt functions like sppt_diff
        if ( isTRUE(all.equal(sppt_func, sppt_diff))) { # Check using sppt_diff
          output[["mean.similarity"]]        <- c(output[["mean.similarity"]], mean((spdf$similarity)))
          output[["mean.similarity.robust"]] <- c(output[["mean.similarity.robust"]], mean((spdf$similarity.robust)))
        } else {
          output[["mean.similarity"]] <-        c(output[["mean.similarity"]], NA)
          output[["mean.similarity.robust"]] <- c(output[["mean.similarity.robust"]], NA)
        }
        
        
        # Optionally sometimes useful to keep a reference to the raw results returned by the sppt call (mostly for debugging)
        if (return.sobject) {
          output[["s.object"]] <- append(output[["s.object"]], list(s))
        } else {
          output[["s.object"]] <- c(output[["s.object"]], NA)
        }
        
        # Also useful to know which iteration number and grid shift this is (useful for naming grids)
        output[["iteration"]] <- c(output[["iteration"]], i)
        output[["shift"]] <- c(output[["shift"]], j)
        
      } # for shifting grids
      
    } # for cell sizes
    
    return(output)
    
  } # run.resolution function
  
  iterations <- seq(from = 1,to = N, by = step)
  pout <- map(iterations, run.resolution)

  # Now we have a big list with all of the results and other useful information in it.
  # The pout list has one item for each resolution. Extract the separate parts into their
  # own variables to make it easier to see what's going on. The code below is extra
  # confusing because sapply returns a matrix (resolutions are rows, shifts are columns (or
  # the other way round)) so c() is needed to vectorise the matrix. 
  results <-                c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["results"]] ))
  num.cells <-              c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["num.cells"]] ))
  n.parameter <-            c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["n.parameter"]] ))
  cell.areas <-             c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["cell.areas"]] ))
  rss <-                    c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["rss"]] ))
  r.squared <-              c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["r.squared"]] ))
  rmse <-                   c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["rmse"]] ))
  globalS <-                unlist(c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["globalS"]] )))
  globalS.robust <-         unlist(c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["globalS.robust"]] )))
  mean.similarity        <- c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["mean.similarity"]] ))
  mean.similarity.robust <- c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["mean.similarity.robust"]] ))
  s.object <-               unlist(lapply(X=1:length(pout), FUN=function(x) pout[[x]][["s.object"]] ), recursive = FALSE)
  grid <-                   unlist(lapply(X=1:length(pout), FUN=function(x) pout[[x]][["grid"]] ), recursive = FALSE)
  iteration <-              c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["iteration"]] ))
  shift <-                  c(sapply(X=1:length(pout), FUN=function(x) pout[[x]][["shift"]] ))
  
  # Delete the results that used one single large cell as these don't mean anything
  results[[1]] <- NULL
  num.cells <-      num.cells      [2:length(num.cells)]
  n.parameter <-    n.parameter    [2:length(n.parameter)]
  cell.areas <-     cell.areas     [2:length(cell.areas)]
  rss <-            rss            [2:length(rss)]
  r.squared <-      r.squared      [2:length(r.squared)]
  rmse <-           rmse           [2:length(rmse)]
  globalS <-        globalS        [2:length(globalS)]
  globalS.robust <- globalS.robust [2:length(globalS.robust)]
  mean.similarity <- mean.similarity[2:length( mean.similarity)]
  mean.similarity.robust <- mean.similarity[2:length( mean.similarity.robust)]
  s.object <-       s.object       [2:length(s.object)]
  grid <-           grid           [2:length(grid)]
  iteration <-      iteration      [2:length(iteration)]
  shift <-          shift          [2:length(shift)]
  
  # Sanity check - global errors and other info should be vectors of the same length
  stopifnot(
    length(num.cells) == length(cell.areas) &
      length(num.cells) == length(n.parameter) &
      length(num.cells) == length(rss) &
      length(num.cells) == length(r.squared) &
      length(num.cells) == length(rmse) &
      length(num.cells) == length(globalS) &
      length(num.cells) == length(globalS.robust) &
      length(num.cells) == length(mean.similarity) &
      length(num.cells) == length(mean.similarity.robust) &
      length(num.cells) == length(s.object) &
      length(num.cells) == length(grid) &
      length(num.cells) == length(iteration) &
      length(num.cells) == length(shift)
  )
  
  # Return the results
  r <- list(
    "results" = results,
    "cell.areas" = cell.areas,
    "num.cells" = num.cells,
    "n.parameter" = n.parameter,
    "rss" = rss,
    "r.squared" = r.squared,
    "rmse" = rmse,
    "globalS" = globalS,
    "globalS.robust" = globalS.robust,
    "mean.similarity" = mean.similarity,
    "mean.similarity.robust" = mean.similarity.robust,
    "s.object" = if (return.sobject) s.object else NA,
    "grid" = grid,
    "iteration" = iteration,
    "shift" = shift
  )
  return(r)
  
} # function

## 3. ----
# These three parameters control the algorithm. The easiest way to generate some results relatively quickly
# is to reduce the number of shifts (N.SHIFTS) or to increase the step (STEP).
iterations <- 100        # The number of iterations
shifts <- 5 # The number of times the grid is shifted randomly at each iteration
step <- 2      # The size of increments as the algorithm iterates between 1 and N.

## 4. ----
loop_msea_diff <- function(a, b, c, d, save = TRUE, ...) {
  
  print(d)
  
  output <- msea_EC(points1 = a,
          points2 = b,
          N = iterations,
          n.shifts = shifts,
          mask = c,
          ignore.zeros = TRUE,
          step = step,
          sppt_func = sppt_diff, adj = "none", test = "Fisher")
  
  if(save) {
    saveRDS(output,
          file = paste("~outputs/20/24_grid_cells/diff/24_",
                       d,
                       "_MSEA_diff.rds",
                       sep = ""))
  }
  
  output
  
}

loop_msea <- function(a, b, c, d, save = TRUE, ...) {
  
  print(d)
  
  output <- msea_EC(points1 = a,
                    points2 = b,
                    N = iterations,
                    n.shifts = shifts,
                    mask = c,
                    ignore.zeros = TRUE,
                    step = step)
  
  if(save) {
    saveRDS(output,
            file = paste("~outputs/20/24_grid_cells/24_",
                         d,
                         "_MSEA.rds",
                         sep = ""))
  }
  
  output
  
}



MSEA_inputs <- list(guns_list_shp_evenYears[28:34],
                    guns_list_shp_oddYears[28:34],
                    BG_selection_list$byCaveHull[28:34],
                    names(BG_selection_list$byCaveHull)[28:34])

MSEA_object <- pmap(.l = MSEA_inputs,
                    function(a, b, c, d) 
                      loop_msea(a = a, b = b, c = c, d = d, save = TRUE))

MSEA_diff_object <- pmap(.l = MSEA_inputs,
                    function(a, b, c, d) 
                      loop_msea_diff(a = a, b = b, c = c, d = d, save = TRUE))

saveRDS(MSEA_object,
        "~outputs/~large_files/24_MSEA_object.rds")
saveRDS(MSEA_diff_object,
        "~outputs/~large_files/24_MSEA_diff_object.rds")


iterations <- 200        # The number of iterations
shifts <- 5 # The number of times the grid is shifted randomly at each iteration
step <- 3      # The size of increments as the algorithm iterates between 1 and N.
largeCities <- c(
  # "Baltimore", "Chicago", "Detroit", "Kansas City", 
                 "Los Angeles", 
                 "Nashville", "New York", "Philadelphia", "San Francisco")
MSEA_inputs_largeCities <- list(guns_list_shp_evenYears[largeCities],
                                guns_list_shp_oddYears[largeCities],
                                BG_selection_list$byCaveHull[largeCities],
                                set_names(names(BG_selection_list$byCaveHull))[largeCities])

MSEA_diff_object_largeCities <- pmap(.l = MSEA_inputs_largeCities,
                         function(a, b, c, d) 
                           loop_msea_diff(a = a, b = b, c = c, d = d, save = TRUE))








MSEA_results <- map(MSEA_object,
                    ~ data.frame("num_cells" = .x$num.cells,
                                 "globalS" = .x$globalS))


ggplot(MSEA_results$Chicago,
       aes(x = num_cells,
           y = globalS)) +
  geom_point(color = "black") +
  geom_smooth(method = "loess")

  
# 1. Test using Chicago ----
# BG_selection_list <- readRDS("~outputs/20/22_BG_selection_list.rds")

Chi_2018 <- guns_list_shp_byYear$Chicago$`2018` %>% 
  slice_sample(prop = 0.25)
Chi_2019 <- guns_list_shp_byYear$Chicago$`2019` %>% 
  slice_sample(prop = 0.25)
Chi_shp <- BG_selection_list$byCaveHull$Chicago

test <- as(Chi_2019, "Spatial")

#### Set parameters ----





points1 <- as(Chi_2018, "Spatial")
points2 <- as(Chi_2019, "Spatial")

bb <- bbox(points1 + points2)

# Run each resolution in parallel. The output is a named list with all of the different elements in it
# resolutions is a list of all resolutions to run at




test <- msea_EC(points1 = Chi_2018, points2 = Chi_2019, 
                N = 50, n.shifts = 5, mask = Chi_shp, ignore.zeros = FALSE, step = 2)

plot(x = test$num.cells,
     y = test$globalS)





















test <- msea_EC(points1 = Chi_2018, points2 = Chi_2019, 
                          N = 100, n.shifts = 10, mask = Chi_shp, ignore.zeros = T, step = 1)

results <- data.frame("num_cells" = test_lite$num.cells,
                     "globalS" = unlist(test_lite$globalS))

ggplot(results,
       aes(x = num_cells,
           y = globalS)) +
  geom_point(color = "black") +
  geom_smooth(method = "loess")



## 1. Export as RDS ----
# saveRDS(guns_list_shp_evenYears,
#         "~outputs/20/24_guns_list_shp_evenYears.rds")
# saveRDS(guns_list_shp_oddYears,
#         "~outputs/20/24_guns_list_shp_oddYears.rds")
