#---- Script Metadata #----
# Title: NASA GESDISC Download Copy
# Date: 04/12/2024
# Author: Kyle T. Aune, PhD, MPH
#--------------------------

# This script compiles functions from ssaxe-usgs/modelCollect that are no longer available

#---- Functions #----
#' List NetCDF Variables and Descriptions
#'
#' List all variables in a NetCDF and associated long names and units if available
#' @param ncPaths One or more netCDF files containing the same variable.
#' @param checkVars Check if variable information is the same in all files (slow).
#' @param verbose Should text be printed to console?
#' @return Variable names and information.
#' @export
#' @examples
#' ncdf_info(ncPaths)

ncdf_info <- function(ncPaths,
                      checkVars = F,
                      verbose = F){
  # Load libraries
  tryCatch({
    library("raster")
    library("ncdf4")
  }, warning = function(w){
    stop("Must install packages 'raster' and 'ncdf4'")
  })
  # function to apply to each layer
  nc_variables <- function(x){
    tryCatch({
      # open nc
      nc <- ncdf4::nc_open(x)
      # Get variable info
      meta_out <- do.call("rbind",
                          lapply(X = nc$var,
                                 FUN = function(y){
                                   # names of list y
                                   y_nm <- names(y)
                                   # get variable name
                                   if ("name" %in% y_nm){
                                     name_out <- y$name
                                   }else{
                                     name_out <- NA
                                   }
                                   # Get Long Name
                                   if ("longname" %in% y_nm){
                                     longname_out <- y$longname
                                   }else{
                                     longname_out <- NA
                                   }
                                   # Get units
                                   if ("units" %in% y_nm){
                                     units_out <- y$units
                                   }else{
                                     units_out <- NA
                                   }
                                   # return as data.frame
                                   data.frame(variable = name_out,
                                              long_name = longname_out,
                                              units = units_out,
                                              stringsAsFactors = F)
                                   
                                 }))
      rownames(meta_out) <- NULL
      # close connection to netcdf
      ncdf4::nc_close(nc)
      # return data.frame of variable info
      return(meta_out)
    }, error = function(e){
      ncdf4::nc_close(nc)
    })
    
  }
  # Get variable data from each file
  if (checkVars == T && length(ncPaths) > 1){
    if (verbose) cat("Making sure variables are the same throughout all files...\n")
    all_nc <- pbapply::pblapply(X = ncPaths,
                                FUN = nc_variables)
  }
  # Get variable info from first file
  outt <- nc_variables(ncPaths[1])
  if (verbose){
    cat("File variable information:\n")
    print(outt)
  }
  # return
  return(outt)
}


#' Extract and Save to File a Single NetCDF Variable
#'
#' Function to extract a single variable from NetCDF and save to file as a rasterStack.
#' Variable names can be found using 'ncdf_info' function.
#' @param ncPaths One or more netCDF files containing the same variable.
#' @param varname Variable name.
#' @param savePath Path to save rasterStack as.  Not written to file if not provided this arg.
#' @param r_level Variable level if required.
#' @param verbose Show progress bar and text
#' @return Names of files.
#' @export
#' @examples
#' extract_netCDF

extract_netCDF <- function(ncPaths,
                           varname,
                           savePath,
                           r_level = NULL,
                           verbose = T
){
  # Load libraries
  tryCatch({
    library("raster")
    library("ncdf4")
  }, warning = function(w){
    stop("Must install packages 'raster' and 'ncdf4'")
  })
  
  # Raster function
  ras_var <- function(x, y, z = r_level){
    # Get raster variable
    ras_out <- tryCatch({
      if (is.null(z)){
        raster::raster(x, var = y)
      }else{
        raster::raster(x, var = y, level = z)
      }
    }, warning = function(w){
      if (grepl(pattern = '"level" set to 1',
                x = w,
                fixed = T)){
        "Level Warning"
      }else{
        w
      }
    })
    # return warning for levels
    if (class(ras_out) == "RasterLayer"){
      return(ras_out)
    }else{
      if (ras_out == "Level Warning"){
        stop(paste0(
          "Variable selected contains multiple levels.\n",
          "\nPlease see the primary source documentation and",
          "\nselect the appropriate level for argument 'r_level'."
        ))
      }else{
        stop(w)
      }
    }
  }
  
  # Load all rasters, extracting specified variable
  if (verbose){
    ras_stack <- raster::stack(
      pbapply::pblapply(X = ncPaths,
                        FUN = ras_var,
                        y = varname[1],
                        z = r_level)
    )
  }else{
    ras_stack <- raster::stack(
      lapply(X = ncPaths,
             FUN = ras_var,
             y = varname[1],
             z = r_level)
    )
  }
  
  # If savePath provided, write to file
  if (!is.null(savePath)){
    if (verbose) cat("Writing to file...\n")
    raster::writeRaster(ras_stack, filename = savePath)
  }
  # return
  return(ras_stack)
}

#---- getNames #----

#' Extract filename information from EarthData GES DISC URLs
#'
#' Function to extract filenames from [NASA EarthData GES DISC](https://disc.gsfc.nasa.gov/) file download URLs.
#' @param path Path to a text file generated from GES DISC that contains list of links of data file to download.
#' @return Names of files.
#' @export
#' @examples
#' getNames_GESDISC(path = "subset_NLDAS_MOS0125_M_V002_20181109_191725.txt")

getNames_GESDISC <- function(url){
  # Split by "&" symbol
  url_split <- strsplit(url, split = "&", fixed = T)
  # Extract filename and variables and create new name
  return_info <- function(x){
    # find indices of LABEL and VARIABLE
    label_ind <- unlist(lapply(x, function(y){
      grepl(pattern = "LABEL",
            x = y,
            fixed = T)
    }))
    var_ind <- unlist(lapply(x, function(y){
      grepl(pattern = "VARIABLE",
            x = y,
            fixed = T)
    }))
    # extract LABEL and VARIABLE
    label_info <- x[label_ind]
    var_info <- x[var_ind]
    # get basename from LABEL
    label_info <- gsub(pattern = "LABEL=",
                       replacement = "",
                       x = label_info,
                       fixed = T)
    label_info_basename <- tools::file_path_sans_ext(label_info)
    label_info_ext      <- tools::file_ext(label_info)
    # return label_info if no variable information
    if (sum(var_ind) == 0) return(label_info)
    # Get variables from VAR
    var_info <- gsub(pattern = "VARIABLES=",
                     replacement = "",
                     x = var_info,
                     fixed = T)
    var_names <- unlist(strsplit(x = var_info,
                                 split = "%2",
                                 fixed = T))
    var_names <- paste(var_names, collapse = "_")
    # Create final filename
    fn_out <- paste0(label_info_basename,
                     ".",
                     "Variables_",
                     var_names,
                     ".",
                     label_info_ext)
    # return
    return(fn_out)
  }
  # get filenames
  fnames <- unlist(lapply(url_split, return_info))
  # return
  return(fnames)
}

#---- downloadGESDISC #----

#' Download from EarthData GES DISC
#'
#' Function to download data from [NASA EarthData GES DISC](https://disc.gsfc.nasa.gov/) URLs.
#' @param txt_path Path to a text file generated from GES DISC that contains list of links of data file to download.
#' @param dest_folder Folder in which to save downloaded files.  If NULL, saves to working directory.
#' @param base_names Manually-generated, user-defined names for saving downloaded files.  Must be same length as number of files.
#' @param rqr_user_pwd Should an EarthData login username and password be required? Defaults to TRUE.
#' @param subset_list Select individual URLs by number.
#' @param progress_bar If TRUE, uses 'pbapply' package to generate console progress bar.
#' @param cl Cluster generated from parallel::makeCluster() to process downloads over multiple cores.  Defaults to NULL (single-threaded).
#' @return Names of files.
#' @export
#' @examples
#' download_GESDISC(txt_path = "filelist.txt", subset_list = c(2,3,4))

download_GESDISC <- function(txt_path,
                             dest_folder  = NULL,
                             base_names   = NULL,
                             rqr_user_pwd = T,
                             subset_list  = NULL,
                             progress_bar = T,
                             cl           = NULL){
  #- If txt_path supplied, import
  url <- read.table(txt_path, stringsAsFactors = F, header = F)[,1]
  
  #- Ask for username and password
  if (rqr_user_pwd){
    username <- rstudioapi::showPrompt(title = "Username",
                                       message = "Username",
                                       default = "")
    password <- rstudioapi::askForPassword(prompt = "Password")
    upw_insert <- paste0(username, ":", password, "@")
  }else{
    upw_insert <- ""
  }
  
  #- remove README file
  is.readme <- grepl(pattern = "README",
                     x       = url)
  url <- url[!is.readme]
  
  #- Select URLs
  if (!is.null(subset_list)){
    url <- url[subset_list]
  }
  
  #- Create filenames if not supplied
  nFiles <- length(url)
  if (is.null(base_names)){
    tryCatch({
      fnames <- getNames_GESDISC(url)
    }, error = function(e){
      stop(paste0("Filename generation failed.",
                  "\nPlease supply vector of file basenames to argument 'base_names'.",
                  "\nMust be same length as number of files (n=",
                  nFiles,
                  ")."))
    })
  }else{
    if (length(base_names) != length(url)){
      stop(paste0("Argument 'base_names' must be same length as number of download files (n=",
                  nFiles,
                  ")."))
    }
    fnames <- base_names
  }
  
  # If "?" included, remove from there back
  if (any(grepl("?", fnames, fixed=T))){
    fnames2 <- strsplit(fnames, "?", fixed = T)
    fnames2 <- do.call("c", lapply(fnames2, function(x){x[1]}))
    fnames <- fnames2
  }
  
  #- Assign folder name
  if (!is.null(dest_folder)){
    # add "/" if necessary
    if (substr(x     = dest_folder,
               start = nchar(dest_folder),
               stop  = nchar(dest_folder)) != "/"){
      dest_folder <- paste0(dest_folder, "/")
    }
    # concatenate filenames
    fnames <- paste0(dest_folder, fnames)
  }else{
    stop("Please specify a destination folder.")
  }
  
  #- If files already exist, remove from list
  if (any(file.exists(fnames))){
    already_dled <- file.exists(fnames)
    cat(paste0("Ignoring files from download list that already exist...\n"))
    cat(paste0(sum(already_dled), "/", length(fnames), " files ignored.\n"))
    fnames <- fnames[!already_dled]
    url    <- url[!already_dled]
  }
  
  #- Insert username/password information
  url_out <- unlist(
    lapply(X = url,
           FUN = function(x, y){
             if (grepl("https://", x, fixed = T)){
               spl_tail <- strsplit(x = x,
                                    split = "https://",
                                    fixed = T)[[1]][2]
               return(
                 paste0("https://", y, spl_tail)
               )
             }else{
               return(x)
             }
           },
           y = upw_insert
    )
  )
  if (identical(url, url_out)){
    warning("URLs supplied do not contain 'https:// and therefore username and password are not applied.")
  }
  
  #- Download
  if (length(url_out) == 1){
    download.file(url      = url_out[1],
                  destfile = fnames[1],
                  mode     = "curl",
                  quiet    = F)
  }else{
    # Combine into df
    mat <- cbind(url_out, fnames)
    # apply download function over matrix with progress bar
    if (progress_bar){
      tf <- pbapply::pbapply(X      = mat,
                             MARGIN = 1,
                             FUN    = function(x){
                               download.file(url      = x[1],
                                             destfile = x[2],
                                             mode     = "wb",
                                             quiet    = T)
                             },
                             cl = cl)
    }else{
      if (is.null(cl)){
        # apply download function over matrix
        tf <- apply(X      = mat,
                    MARGIN = 1,
                    FUN    = function(x){
                      download.file(url      = x[1],
                                    destfile = x[2],
                                    mode     = "wb",
                                    quiet    = T)
                    })
      }else{
        parallel::parApply(cl = cl,
                           X      = mat,
                           MARGIN = 1,
                           FUN    = function(x){
                             download.file(url      = x[1],
                                           destfile = x[2],
                                           mode     = "wb",
                                           quiet    = T)
                           })
      }
    }
  }
  
  #- Return names
  invisible(fnames)
}