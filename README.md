# Azavea Summer of Maps
This is the repository for the [State Gun Law Project](https://stategunlawproject.org/) / [PennPraxis](https://www.design.upenn.edu/pennpraxis/home) project for the 2020 edition of the [Azavea Summer of Maps Fellowship](https://www.summerofmaps.com/).

* **Fellow:** [Eugene Chong](https://e-chong.github.io/)
* [Final presentation recording](https://drive.google.com/open?id=19BTBXISt9iR6OxC6LDHMqVG-RAwh1W1K)
* [Project description](https://www.summerofmaps.com/projects/shortlist/2020-pennpraxis)

# File Structure

## Data

All SGLP-provided raw data are saved in the Dropbox.

## Code

All scripts can be run in the order that they are numbered.

Note that Census and Google API Keys (for geocoding Virginia Beach only) are loaded from local files in `00 - Admin.R`. Readers running the code here will need to supply their own keys.

### Markdowns

**00 - Working markdown.rmd:** Draft visuals and observations from data exploration.

### Scripts

**00 - Admin.R:** Load packages and define filepath for Dropbox data. Run this whenever starting an analysis task.

**01 - Utility Functions.R:** Any functions written in this project will be saved here.

**02 - Re-construct alldatahierarchies.R:** Re-constructs the `alldata.csv` and `alldatahierarchies.csv` files originally provided by SGLP, which used the raw versions of each city's data, using the cleaned versions of each city's data instead.

**1\_.R:** Read in or collect raw data

**2\_.R:** Clean data for analysis

**3\_.R:** Plots and analysis

### Outputs

#### Plots
All plots, including the exported PDFs used for the final deliverable maps, are exported to this folder.

#### Intermediate objects

All intermediate objects are saved as `.rds` and are saved using the following format.

**Format:** `/~outputs/[scriptRange]/[script#]_[objectName].rds`

**Example:** `/~outputs/10/11_gun_crimes_df.rds` is object `gun_crimes_df` produced in script `11 - Read crime data.R`.