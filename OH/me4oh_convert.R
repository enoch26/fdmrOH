# Convert ME4OH NetCDF to CSV

library("ncdf4")
library("tidyverse")

read_var <- function(nc_data, var_name) {  # Read NetCDF variable and preprocess fill values.
    # Read in the NetCDF variable.
    var <- ncvar_get(nc_data, var_name)
    # Replace fill values with NA if fill value is defined.
    fillvalue <- ncatt_get(nc_data, var_name, "_FillValue")
    if (fillvalue$hasatt) {
        var[var==fillvalue$value] <- NA
    }
    # Replace any instance of NaN with NA.
    var[is.nan(var)] <- NA

    # Return the preprocessed variable.
    return(var)

}

nice_name <- function(nc_data, var_name) {  # Construct a nice name for a variable.
    name <- ncatt_get(nc_data, var_name, "short_name")$value
    units <- ncatt_get(nc_data, var_name, "units")$value
    result <- sprintf("%s (%s)", name, units)
}

preprocess_me4oh_profiles <- function(ncfile) {  # Preprocess ME4OH NetCDF file and return data frame.
    
    # Get NetCDF data.
    our_nc_data <- nc_open(nc_file)

    # Get coordinates (by profile).
    lat <- ncvar_get(our_nc_data, "en4_lat") # Latitude
    lon <- ncvar_get(our_nc_data, "en4_lon") # Longitude
    time <- ncvar_get(our_nc_data, "en4_ymd") # Year, Month, Day
    decimal_year <- ncvar_get(our_nc_data, "en4_ydec") # Decimal years
    platform_number <- read_var(our_nc_data, "en4_platform_number") # Platform number

    # Get coordinates (by level)
    ts_z <- read_var(our_nc_data, "ts_z") # Depth of level.

    # Counts of coordinate lengths.
    number_of_levels <- dim(ts_z)
    number_of_profiles <- dim(lat)

    # Separate time components.
    year <- time[1, ]
    month <- time[2, ]
    day <- time[3, ]

    # Utility variables to identify profiles or levels by index.
    number_of_profile <- c(1:number_of_profiles)
    level <- c(1:number_of_levels)

    # Get sea surface temperature values.
    sst <- read_var(our_nc_data, "sst")
    sst_name <- nice_name(our_nc_data, "sst")

    # Data frame of by profile meta data.
    by_profile_df <- data.frame(cbind(number_of_profile, lat, lon, platform_number, year, month, day, decimal_year, sst))

    # Data fram of by level meta data.
    by_level_df <- data.frame(cbind(level, ts_z))
    meta_df <- merge(by_level_df, by_profile_df, by = NULL)
    
    # Get profile by level data blocks.

    # Get temperature values.
    temp <- read_var(our_nc_data, "temp")
    temp_name <- "potential_temperature (degree Celcius)" # temp_name <- nice_name(our_nc_data, "temp") # set manually as typo in netcdf short name

    # Get salinity values.
    salt <- read_var(our_nc_data, "salt")
    salt_name <- nice_name(our_nc_data, "salt")

    # Reshape to 1D and combine into data frame.
    temp <- as.vector(temp)
    salt <- as.vector(salt)
    data_df <- data.frame(cbind(temp, salt))
    colnames(data_df) <- c(temp_name, salt_name)

    # Merge coordinate meta data with observation values.
    me4oh_df <- cbind(meta_df, data_df)

    # Drop NA rows.
    # me4oh_df <- na.omit(me4oh_df)

    # Reorder columns to nicer order.
    column_order <- c("number_of_profile","platform_number","year","month","day", "decimal_year", "lat","lon","level","ts_z","sst","potential_temperature (degree Celcius)","practical_salinity (psu)")
    me4oh_df <- me4oh_df[, column_order]

    # Mask to top level only for initial development.
    me4oh_df <- me4oh_df[me4oh_df$level==1, ]

    return(me4oh_df)
}


preprocess_me4oh_dohc <- function(nc_file) {  # Preprocess ME4OH NetCDF file and return data frame.
    
    # Get NetCDF data.
    our_nc_data <- nc_open(nc_file)

    # Get coordinates (by profile).
    lat <- ncvar_get(our_nc_data, "en4_lat") # Latitude
    lon <- ncvar_get(our_nc_data, "en4_lon") # Longitude
    time <- ncvar_get(our_nc_data, "en4_ymd") # Year, Month, Day
    decimal_year <- ncvar_get(our_nc_data, "en4_ydec") # Decimal years
    platform_number <- read_var(our_nc_data, "en4_platform_number") # Platform number

    # Get coordinates (by level)
    ts_z <- read_var(our_nc_data, "ts_z") # Depth of level.

    # Counts of coordinate lengths.
    number_of_levels <- dim(ts_z)
    number_of_profiles <- dim(lat)

    # Separate time components.
    year <- time[1, ]
    month <- time[2, ]
    day <- time[3, ]

    # Utility variables to identify profiles or levels by index.
    number_of_profile <- c(1:number_of_profiles)
    level <- c(1:number_of_levels)

    # Get sea surface temperature values.
    sst <- read_var(our_nc_data, "sst")
    sst_name <- nice_name(our_nc_data, "sst")
    
    # Data frame of by profile meta data.
    by_profile_df <- data.frame(cbind(number_of_profile, platform_number, year, month, day, decimal_year, lat, lon, sst))

    # Data frame for dohc at different integration levels
    dohc <- read_var(our_nc_data, "dohc")
    dohc_mask_by_en4_maxdepth <- read_var(our_nc_data, "dohc_mask_by_en4_maxdepth") == 1 # Mask by profile ID by full depth level coverage.

    dohc_L1 <- dohc[1, ]
    dohc_mask_L1 <- dohc_mask_by_en4_maxdepth[1, ]
    dohc_L2 <- dohc[2, ]
    dohc_mask_L2 <- dohc_mask_by_en4_maxdepth[2, ]
    dohc_L3 <- dohc[3, ]
    dohc_mask_L3 <- dohc_mask_by_en4_maxdepth[3, ]

     # Build data frame
    dohc_df <- data.frame(dohc_L1, dohc_mask_L1, dohc_L2, dohc_mask_L2, dohc_L3, dohc_mask_L3)

    # Merge coordinate meta data with observation values.
    me4oh_df <- cbind(by_profile_df, dohc_df)

    return(me4oh_df)
}

# Set input and output directories - modify these to set input and output locations
in_dir <-"ME4OH/data/en4.1.1/1979-2014/full/update"
out_dir <- "ME4OH"

for (year in c(1979:2014)) {
    for (month in c(1:12)) {
        datestr <- sprintf("%04d%02d", year, month)
        in_file <- file.path(in_dir, sprintf("ofam3-jra55.all.EN.4.1.1.f.profiles.g10.%s.update.extra.nc", datestr))
        out_file <- file.path(out_dir, sprintf("me4oh_dohc.%s.csv", datestr))

        print(in_file)
        print(out_file)

        df <- preprocess_me4oh_dohc("/data/users/hadcc/ME4OH/data/en4.1.1/1979-2014/full/update/ofam3-jra55.all.EN.4.1.1.f.profiles.g10.197901.update.extra.nc")
        write.csv(df, out_file, row.names=FALSE)
    }

}

# An option to save as rds object
i <- 0
for (year in c(2005:2005)) {
  for (month in c(1:12)) {
    datestr <- sprintf("%04d%02d", year, month)
    in_file <- file.path(in_dir, sprintf("ofam3-jra55.all.EN.4.1.1.f.profiles.g10.%s.update.extra.nc", datestr))
    
    print(in_file)
    
    if (i == 0){
      df <- preprocess_me4oh_dohc(in_file)
      i = i + 1
    } else {
      df2 <- preprocess_me4oh_dohc(in_file)
      df <- rbind(df, df2)
    }
  }
}

saveRDS(df, file.path(out_dir, "me4_dohc.2005.rds"))
