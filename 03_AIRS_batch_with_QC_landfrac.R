# AIRS L2 RetSup_IR -> 3 linked Parquet tables, preserving matching QC fields.
# Run in a FRESH R.app session: source('/path/to/03_AIRS_batch_with_QC.R')
# Requires R packages reticulate, arrow; Miniforge nasa_airs with pyhdf.

PYTHON <- '/Users/gaellemuller-greven/miniforge3/envs/nasa_airs/bin/python'
INPUT_ROOT <- '/Users/gaellemuller-greven/spaceapps2026_data'
OUTPUT_ROOT <- paste0(INPUT_ROOT, '_parquet')
RUN_ALL_FILES <- TRUE  # First test one granule; change to TRUE after checking landFrac.
OVERWRITE <- FALSE       # TRUE forces regeneration of all selected granules.

library(reticulate)
use_python(PYTHON, required=TRUE)  # before Python initializes
library(arrow)
SD <- reticulate::import('pyhdf.SD')$SD
SDC <- reticulate::import('pyhdf.SD')$SDC

read_sds <- function(h, name, clean_fill=TRUE) {
  s <- h$select(name)
  on.exit(s$endaccess(), add=TRUE)
  x <- s$get()
  if (clean_fill && is.numeric(x)) {
    attrs <- tryCatch(reticulate::py_to_r(s$attributes()), error=function(e) list())
    fill <- attrs[['_FillValue']]
    if (is.null(fill)) fill <- -9999
    x[x == as.numeric(fill)[1]] <- NA
  }
  x
}

# R arrays are column-major; these keys match as.vector() of 45x30xN arrays.
keys2 <- function(ntrack, nxtrack) data.frame(
  GeoTrack=rep(seq_len(ntrack), times=nxtrack),
  GeoXTrack=rep(seq_len(nxtrack), each=ntrack))
keys3 <- function(ntrack, nxtrack, nlevel, index_name) {
  k <- keys2(ntrack,nxtrack)
  out <- k[rep(seq_len(nrow(k)), times=nlevel),,drop=FALSE]
  out[[index_name]] <- rep(seq_len(nlevel), each=nrow(k))
  rownames(out) <- NULL
  out
}

# Preserve QC integers/codes without applying numeric fill replacement.
# Only attach QC if its shape matches the corresponding measurement table.
add_qc <- function(tab, h, available, dataset, out_name, expected_dims) {
  qname <- paste0(dataset, '_QC')
  if (!(qname %in% available)) return(tab)
  q <- read_sds(h, qname, clean_fill=FALSE)
  if (!identical(as.integer(dim(q)), as.integer(expected_dims))) {
    warning('QC shape differs; not attaching ',qname, ': ',
            paste(dim(q),collapse='x'), ' vs ', paste(expected_dims,collapse='x'))
    return(tab)
  }
  tab[[paste0(out_name,'_QC')]] <- as.vector(q)
  tab
}

extract_one <- function(path, overwrite=FALSE) {
  folder <- basename(dirname(path))
  m <- regmatches(folder,regexec('^([0-9]{2})_(day|night)$',folder))[[1]]
  if (length(m)!=3L) stop('Unexpected folder: ',folder)
  stem <- sub('\\.hdf$','',basename(path),ignore.case=TRUE)
  date_match <- regmatches(stem,regexec('^AIRS\\.([0-9]{4})\\.([0-9]{2})\\.([0-9]{2})\\.',stem))[[1]]
  if (length(date_match)!=4L) stop('Unexpected filename: ',stem)
  granule_date <- as.Date(paste(date_match[2:4],collapse='-'))
  dest <- file.path(OUTPUT_ROOT,folder)
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  outputs <- file.path(dest,paste0(stem,c('_profiles.parquet','_observations.parquet','_clouds.parquet')))
  # New version marker deliberately absent from old script outputs, so the
  # previously created test granule is rebuilt with QC on first batch run.
  marker <- file.path(dest,paste0(stem,'_qc_v2.complete'))
  if (!overwrite && file.exists(marker) && all(file.exists(outputs))) return('SKIPPED')

  h <- SD(path,SDC$READ)
  on.exit(try(h$end(),silent=TRUE),add=TRUE)
  available <- names(h$datasets())
  lat <- read_sds(h,'Latitude'); lon <- read_sds(h,'Longitude'); tai <- read_sds(h,'Time')
  dims2 <- dim(lat)
  if (length(dims2)!=2L || !identical(dim(lon),dims2) || !identical(dim(tai),dims2))
    stop('Latitude/Longitude/Time dimensions disagree')
  n <- prod(dims2)
  obs <- cbind(data.frame(
    granule=rep(stem,n), sampling_month=rep(as.integer(m[2]),n), time_of_day=rep(m[3],n),
    granule_date=rep(granule_date,n), year=rep(as.integer(date_match[2]),n),
    month=rep(as.integer(date_match[3]),n), day=rep(as.integer(date_match[4]),n)),
    keys2(dims2[1],dims2[2]))
  # data.frame above has one row; cbind recycles its metadata across observations.
  obs$latitude <- as.vector(lat)
  obs$longitude <- as.vector(lon)
  obs$time_tai_seconds_since_1993 <- as.vector(tai) # raw TAI; NOT UTC
  observation_vars <- c(landFrac='landFrac',CO2ppmv='CO2ppmv',CldFrcTot='CldFrcTot',
                        TSurfStd='TSurfStd',PSurfStd='PSurfStd',
                        totH2OStd='totH2OStd',totO3Std='totO3Std',
                        CO_total_column='CO_total_column',
                        CH4_total_column='CH4_total_column',
                        GP_Surface='GP_Surface',GP_Tropopause='GP_Tropopause')
  if (!('landFrac' %in% available)) warning('landFrac not present in HDF granule: ',stem,'; land/water splits will not be possible for this granule')
  for (v in names(observation_vars)) {
    if (!(v %in% available)) next
    a <- read_sds(h,v)
    if (!identical(dim(a),dims2)) {warning('Skipping shape mismatch: ',v);next}
    col <- observation_vars[[v]]
    obs[[col]] <- as.vector(a)
    obs <- add_qc(obs,h,available,v,col,dims2)
  }

  profile_vars <- c(TAirSup='temperature_K',GP_HeightSup='geopotential_height',
                    H2OMMRLevSup='water_vapor_mmr',O3VMRLevSup='ozone_vmr',
                    COVMRLevSup='co_vmr',CH4VMRLevSup='methane_vmr')
  first <- read_sds(h,names(profile_vars)[1]); dims3 <- dim(first)
  if (length(dims3)!=3L || !identical(as.integer(dims3[1:2]),as.integer(dims2)))
    stop('Unexpected profile dimensions')
  prof <- cbind(data.frame(granule=rep(stem,n*dims3[3])),keys3(dims2[1],dims2[2],dims3[3],'pressure_level_index'))
  for (v in names(profile_vars)) {
    if (!(v %in% available)) next
    a <- if (v==names(profile_vars)[1]) first else read_sds(h,v)
    if (!identical(dim(a),dims3)) {warning('Skipping shape mismatch: ',v);next}
    col <- profile_vars[[v]]
    prof[[col]] <- as.vector(a)
    prof <- add_qc(prof,h,available,v,col,dims3)
  }
  pressure_names <- c('pressSupp','XtraPressureLev:L2_Support_atmospheric&surface_product')
  for (v in pressure_names) if (v %in% available) {
    pressure <- tryCatch(read_sds(h,v),error=function(e) NULL)
    if (!is.null(pressure) && length(pressure)==dims3[3]) {
      prof$pressure_hPa <- rep(as.vector(pressure),each=n)
      break
    }
  }
  if (!('pressure_hPa' %in% names(prof))) stop('Missing pressure coordinate')

  top <- read_sds(h,'PCldTopStd'); cloud_dims <- dim(top)
  if (length(cloud_dims)!=3L || !identical(as.integer(cloud_dims[1:2]),as.integer(dims2)))
    stop('Unexpected cloud dimensions')
  clouds <- cbind(data.frame(granule=rep(stem,n*cloud_dims[3])),keys3(dims2[1],dims2[2],cloud_dims[3],'cloud_layer_index'))
  cloud_vars <- c(PCldTopStd='cloud_top_pressure',TCldTopStd='cloud_top_temperature')
  for (v in names(cloud_vars)) {
    if (!(v %in% available)) next
    a <- if (v=='PCldTopStd') top else read_sds(h,v)
    if (!identical(dim(a),cloud_dims)) {warning('Skipping shape mismatch: ',v);next}
    col <- cloud_vars[[v]]
    clouds[[col]] <- as.vector(a)
    clouds <- add_qc(clouds,h,available,v,col,cloud_dims)
  }
  stopifnot(nrow(obs)==n,nrow(prof)==n*dims3[3],nrow(clouds)==n*cloud_dims[3])

  # All three files are written first; marker is written LAST. If interrupted,
  # the granule is rebuilt next time. Existing outputs from older scripts are
  # replaced only after successful writing of temporary Parquet files.
  tmp <- paste0(outputs,'.writing')
  on.exit(unlink(tmp),add=TRUE)
  for (i in seq_along(tmp)) arrow::write_parquet(list(prof,obs,clouds)[[i]],tmp[i])
  for (i in seq_along(outputs)) {
    if (file.exists(outputs[i])) unlink(outputs[i])
    if (!file.rename(tmp[i],outputs[i])) stop('Could not rename ',tmp[i])
  }
  writeLines(paste('qc_v2',format(Sys.time(),tz='UTC',usetz=TRUE)),marker)
  'OK'
}

all_files <- sort(list.files(INPUT_ROOT,pattern='\\.hdf$',recursive=TRUE,
                             full.names=TRUE,ignore.case=TRUE))
all_files <- all_files[grepl('/[0-9]{2}_(day|night)/',all_files)]
if (!length(all_files)) stop('No HDF4 granules found under ',INPUT_ROOT)
if (!RUN_ALL_FILES) all_files <- all_files[1]
dir.create(OUTPUT_ROOT,recursive=TRUE,showWarnings=FALSE)
cat('Selected',length(all_files),'granule(s); RUN_ALL_FILES =',RUN_ALL_FILES,'\n')
log_path <- file.path(OUTPUT_ROOT,'batch_qc_v2_log.tsv')
for (i in seq_along(all_files)) {
  f <- all_files[i]
  result <- tryCatch(extract_one(f,overwrite=OVERWRITE),error=function(e) {
    message('ERROR: ',basename(f),': ',conditionMessage(e))
    paste0('ERROR: ',gsub('[\t\r\n]',' ',conditionMessage(e)))
  })
  cat(format(Sys.time(),'%Y-%m-%d %H:%M:%S'), '\t', f, '\t',result,'\n',
      file=log_path,append=TRUE,sep='')
  if (i %% 50L==0L || i==length(all_files)) cat('Progress:',i,'/',length(all_files),'\n')
  rm(result)
  if (i %% 20L==0L) gc(verbose=FALSE)
}
cat('Finished. Log:',log_path,'\n')
