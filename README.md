# SGLP
PennPraxis State Gun Law Project for Azavea Summer of Maps Fellowship 2020

# Organization

## Data

All SGLP-provided raw data are saved in the Dropbox.

## Code

### Markdowns

**00 - Working markdown.rmd:** Use for writing and testing code

### Scripts

**00 - Admin.R:** Load packages and define filepath for Dropbox data. Run this whenever starting an analysis task.

**1\_.R:** Read in or collect raw data

**2\_.R:** Clean data for analysis

**3\_.R:** Exploratory plots and analysis

### Outputs (incl. intermediate objects

**Format:** `/~outputs/[scriptRange]/[script#]_[objectName].rds`

**Example:** `/~outputs/10/11_gun_crimes_df.rds` is object `gun_crimes_df` produced in script 11.