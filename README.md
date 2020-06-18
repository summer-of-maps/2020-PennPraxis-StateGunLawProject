# SGLP
PennPraxis State Gun Law Project for Azavea Summer of Maps Fellowship 2020

# Organization

## Data

All SGLP-provided raw data are saved in the Dropbox.

## Code

Note that Census and Google API Keys (for geocoding Virginia Beach only) are loaded from local files in `00 - Admin.R`. Readers running the code here will need to supply their own keys.

### Markdowns

**00 - Working markdown.rmd:** Use for writing and testing code

### Scripts

**00 - Admin.R:** Load packages and define filepath for Dropbox data. Run this whenever starting an analysis task.

**01 - Utility Functions.R:** Any functions written in this project will be saved here.

**02 - Re-construct alldatahierarchies.R:** Re-constructs the `alldata.csv` and `alldatahierarchies.csv` files originally provided by SGLP, which used the raw versions of each city's data, using the cleaned versions of each city's data instead.

**1\_.R:** Read in or collect raw data

**2\_.R:** Clean data for analysis

**3\_.R:** Exploratory plots and analysis

### Outputs (incl. intermediate objects

**Format:** `/~outputs/[scriptRange]/[script#]_[objectName].rds`

**Example:** `/~outputs/10/11_gun_crimes_df.rds` is object `gun_crimes_df` produced in script 11.