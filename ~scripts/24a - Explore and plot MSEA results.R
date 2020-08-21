##########################################################################
## MSEA Sandbox
# Create plots for cell size analysis
##########################################################################

city <- readRDS("C:/Users/echong/Documents/SummerOfMaps/PennPraxis/PennPraxis_StateGunLawProject/~outputs/20/24_grid_cells/diff/24_Baltimore_MSEA_diff.rds")

# guns_list_shp_evenYears <- readRDS("C:/Users/echong/Documents/SummerOfMaps/PennPraxis/PennPraxis_StateGunLawProject/~outputs/20/24_guns_list_shp_evenYears.rds")
years_byCity <- readRDS("C:/Users/echong/Documents/SummerOfMaps/PennPraxis/PennPraxis_StateGunLawProject/~outputs/20/23_years_byCity.rds")
guns_list_shp <- readRDS("C:/Users/echong/Documents/SummerOfMaps/PennPraxis/PennPraxis_StateGunLawProject/~outputs/20/21_guns_list_shp.rds")

## Expected frequency ----
EXPECTED <- 5 # Set the minimum number of expected points

# Takes the output from sppt and adds a new column for the expected frequency 
expected.frequency.calc <- function(s) {
  
  # Need the column marginal with the fewest crimes to be conservative
  col.marginal <- if ( sum(s$points1_cnt) < sum(s$points2_cnt)) as.numeric(sum(s$points1_cnt)) else as.numeric(sum(s$points2_cnt))
  
  # Total number of events in total
  grand.total <- as.numeric(sum(s$points1_cnt) + sum(s$points2_cnt))
  
  # Join together points1 and points2
  bind <- cbind(s$points1_cnt, s$points2_cnt)
  
  # Now calculate expected as row_marginal * col_marginal / grand_total
  s$expected <- unlist(lapply( X = 1:nrow(s), FUN = function(x) {
    row.marginal <- bind[x,1] + bind[x,2] # (points1 + points2)
    return( round(row.marginal * col.marginal / grand.total ))
  } ) )
  return(s)
}

num.grids <- length(city$cell.areas) # The total number of grids calculated

# Go through each s.object and replace it with one that has the expected frequency column added
for (j in 1:num.grids) {
  city$s.object[[j]] <- expected.frequency.calc(city$s.object[[j]])
}


### Look at how expected frequency changes with number of cells ----
n.cells <- city[["num.cells"]]
grids <- city$s.object # need the output s objects to get the expected value
# Get the mean power of each iteration:
mean.expected <- sapply(X = grids, FUN=function(x) { mean(x$expected)} ) 

median.expected <- sapply(X = grids, FUN=function(x) { median(x$expected)} ) 

tmp <- data.frame(n.cells, mean.expected, median.expected)

ggplot(tmp, aes(x = n.cells, y = mean.expected)) +
  geom_point() +
  labs(title = "Average expected crimes per cell")

## See what the proportion of cells with sufficient expected frequency looks like for the different crimes types.----
n.cells <- city[["num.cells"]]
grids <- city$s.object # need the output s objects to get the expected value
ratio <- sapply(X = grids, FUN=function(x) {
      length(which(x$expected >= EXPECTED)) / length(x$expected)
    } ) 

tmp <- data.frame(n.cells, mean.expected, ratio)
tmp_plot1 <- ggplot(tmp, aes(x = n.cells, y = ratio)) +
  geom_point() +
  geom_smooth(method = "loess") +
  # geom_vline(xintercept = 1200,
  #            col = "red") +
  labs(title = "Proportion of cells with expected points > 5") +
  plotTheme() +
  xlim(0, 6000)



## global S v number of ALL cells, including 0s
n.cells <- city[["num.cells"]]
globalS <- city$globalS # need the output s objects to get the expected value
tmp <- data.frame(n.cells, globalS)

ggplot(tmp,
       aes(x = n.cells,
           y = globalS)) +
  geom_point(color = "black") +
  geom_smooth(method = "loess") +
  # labs(title = "Mean Similarity, including all cells, even those with low expected frequency") +
  labs(title = "Mean Similarity of all cells") +
  plotTheme() +
  xlim(0, 6000)


## And what about the raw numbers of cells. This is possibly useful because it gives us an idea about how much information we're using to determine similarity, but can be misleading because it ignores the fact that the total number of cells increases with resolution, so the number with expected > 5 increases as well.----
n.cells <- city[["num.cells"]]
grids <- city$s.object # need the output s objects to get the expected value
num <- sapply(X = grids, FUN=function(x) { length(which(x$expected >= EXPECTED ))} ) 
tmp <- data.frame(n.cells, mean.expected, num)

ggplot(tmp, aes(x = n.cells, y = num)) +
  geom_point()

## Map similarity after Expected Frequency Analysis----
statistic <- "similarity" # (note 'similarity' rather than 'mean.similarity' because we're looking at the similarity on a cell-level, not mean of the whole grid)
options(scipen=10)

cell <- 240

n.cells <- city[["num.cells"]][cell]
sq.area <- city[["cell.areas"]][cell]
# grid <- city$grid[[cell]] # need the output s objects to get the expected value
grid <- city$s.object[[cell]] # need the output s objects to get the expected value

cols <- sapply( X = 1:nrow(grid), FUN = function(i) {
  expected <- grid$expected[i]
  sim <- grid[[statistic]][1]
  if (expected < EXPECTED) return("#D3D3D3") # insufficient expected number of crimes
  if (sim==0) return("#d8b365") # not similar
  if (sim==1) return("#5ab4ac") # similar
  return ("#FF0000") # an error
} )

tmp <- cbind(grid, cols) %>% 
  mutate(sim_expectedOnly = ifelse(expected < EXPECTED, 
                                      NA,
                                      similarity))
ggplot() +
  geom_sf(data = city$grid[[cell]], fill = NA, size = 0.01) +
  geom_sf(data = tmp, size = 0.01, aes(fill = factor(sim_expectedOnly))) +
  scale_fill_manual(na.value = NA,
                    values = c("#d8b365", 
                               "#5ab4ac")) +
  mapTheme()

## Graph mean similarity (ignoring those with insufficient expected frequencies) ----
num.grids <- length(city$cell.areas) # The total number of grids calculted
mean.similarities <- c() #ia vertor holding the mean similarity (excluding low expected frequency) of each grid
  
# For each grid
for (j in 1:num.grids) {
    ef <- city$s.object[[j]]$expected # Vector of expected frequencies
    # Calculate the mean similarity, disregarding those with < 5 frequency
    mean.similarities[j] <- mean(city$s.object[[j]]$similarity[which(ef>=EXPECTED)])
  }
  
  # Should be one calculation for each iteration
  length(city$cell.areas) == length(mean.similarities)
  
  # Append new similarity to the results
  city$mean.similarity.excluding.low.ef <- mean.similarities
  

  # ITERATIONS.TO.MAP shows  the iteraitons that I am aiming for  (defined earlier)
  # grids.to.map shows the grids that match these iterations (one of the many at that particular iteration anyway)
  
  # Make some lists so that the same plotting code can loop
  # crime.types = vapply(X=all.diff, FUN=function(x)x$name, FUN.VALUE = character(1)) # The crime types (4)
  
  # Make some lists so that the same plotting code can loop 
  # colours  <- brewer.pal(4, "Set2") # Colours for the points and lines
  
  s.type = "mean.similarity.excluding.low.ef" # Note that this is only calculated when using sppt_diff
  

    r <- city # This is the results of the test for the crime type
    x <- r[["num.cells"]] 
    y <- r[[s.type]] # The S Index (either normal or robust)
    df = data.frame("NumCells"=r[["num.cells"]], "NewSimilarity"=r[[s.type]] )
    
    plots <- ggplot(df, aes(NumCells, NewSimilarity)) +
      geom_hex(bins=20, show.legend = FALSE) +
      scale_fill_gradientn(colours=c("white","red"),name = "Frequency")+
      #geom_point(size=0.5, color=colours[i]) +
      geom_point(size=0.8, color='black') + 
      geom_smooth(method="loess", se=TRUE, level=0.99, colour="black")+
      ggtitle(paste0("Mean Similarity","\n(Excluding cells with low expected frequency)"))+
      ylab("Mean Similatiry")+
      xlab("Number of Cells in the Grid")
    # Add horizontal lines showing which resolutions are being mapped


    tmp_plot2 <- ggplot(df[10:334,], aes(NumCells, NewSimilarity)) +
      geom_hex(bins=20, show.legend = FALSE) +
      scale_fill_gradientn(colours=c("white","red"),name = "Frequency")+
      #geom_point(size=0.5, color=colours[i]) +
      geom_point(size=0.8, color='black') + 
      geom_smooth(method="loess", se=TRUE, level=0.99, colour="black")+
      ggtitle(paste0("Mean Similarity","\n(Excluding cells with low expected frequency)"))+
      # geom_vline(xintercept = 1200,
      #            col = "red") +
      ylab("Mean Similatiry")+
      xlab("Number of Cells in the Grid") +
      xlim(0, 6000) +
      # ylim(0.9, 0.96) +
      plotTheme()

grid.arrange(tmp_plot1, tmp_plot2, ncol = 1)

results_tmp <- city$grid[[65]] %>% mutate(
  count_2017 = lengths(st_intersects(., guns_list_shp_byYear$Baltimore$`2017`)),
  count_2018 = lengths(st_intersects(., guns_list_shp_byYear$Baltimore$`2018`))
) %>% 
  gather(key = "year",
         value = "count",
         count_2017:count_2018)

tmp_3 <- bind_rows(guns_list_shp_byYear$Baltimore$`2017` %>% 
                     mutate(year = 2017),
                   guns_list_shp_byYear$Baltimore$`2018` %>% 
                     mutate(year = 2018)) %>% 
  .[results_tmp,]

ggplot() +
  geom_sf(data = results_tmp,
          aes(fill = count)) +
  geom_sf(data = tmp_3,
          col = "red") +
  scale_fill_viridis_c() +
  facet_wrap(~ year) +
  mapTheme()