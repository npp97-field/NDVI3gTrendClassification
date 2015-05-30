TrendAnalyzer <- function(region.countries, data.path, output.path, output.format, data.date.start, data.date.end, trendMode, nCores){
        
        # Load region countries data from GADM.org
        region.GADM <- lapply(region.countries ,function(x) raster::getData('GADM', country=x, level=0))
        
        # Change polygons IDs to unique values
        region.set.IDs <- function(sp,i) spChFIDs(sp,paste(i,row.names(sp@data),sep="."))
        
        # Combine SpatialPolygonDataFrame objects
        region.SPDF  <- do.call(rbind,mapply(region.set.IDs,region.GADM,seq_along(region.GADM)))
        
        # Create an empty template
        region.r.ext <- extent(c(-180, 180, -90 , 90)) ## to be used as a mask
        region.r.res <- 1/12 ## resolution from GIMMS 3g specification
        region.r.tpl <- raster(ext = region.r.ext, resolution = region.r.res)
        
        # Rasterize SPDF polygons to raster with 0 value
        region.r <- rasterize(region.SPDF, region.r.tpl, field=0)
        
        #
        # Load GIMMS NDVI 3g data
        #
        
        # Define a raster stack for the NDVI time-series
        region.ts <- stack()
        
        # GIMMS NDVI 3g grid parameters
        data.samples <- 2160 ## or cols
        data.lines <- 4320 ## or rows
        data.size   <- data.samples*data.lines ## number of items
        data.type <- 2 ## 2-byte short
        data.byte.order <- 1 ## 0 for little endian / 1 for big endian
        
        # Check what type of endian the reader should use
        ieee <- if(.Platform$endian=="big") 1 else 0 ## check if this machine use big endian
        data.endian <- if(ieee==data.byte.order | data.byte.order<0) .Platform$endian else "swap"
        
        # Generate dates for the period
        data.dates <- as.yearmon(seq(as.numeric(data.date.start), as.numeric(data.date.end), 1/24))
        data.dates <- c(data.dates, data.date.end) ## Append one more measurement (two per month)
        
        # Format according to GIMMS file naming 
        data.names <- format(data.dates, format="%y%b")
        data.names <- tolower(data.names) ## months to lower case
        
        # Create two periods (a/b) for each month
        data.names[1:length(data.names) %% 2 == 1] <- paste(data.names[1:length(data.names) %% 2 == 1], "15a", sep = "")
        data.names[1:length(data.names) %% 2 == 0] <- paste(data.names[1:length(data.names) %% 2 == 0], "15b", sep = "")
        
        # Create a list of files 
        file.patern <- paste("geo", data.names, ".*-VI3g", sep = "")
        data.files <- lapply(file.patern, function(x) {list.files(data.path, pattern = x, full.names = TRUE)[1]})
        
        message("Loading NDVI3g data...","\n",appendLF=FALSE)
        flush.console()
        
        # Parallel reading of files
        cl <- makeCluster(nCores)
        clusterExport(cl, c("readNDVI3g", "data.files", "data.size", "data.type", "data.endian", 
                            "data.samples", "data.lines", "region.r", "region.SPDF"), envir=environment())
        junk <- clusterEvalQ(cl, c(library(raster), library(rgdal)))
        region.ts <- stack(parSapply(cl,seq(data.files), function(i) { readNDVI3g(data.file=data.files[[i]],
                                   data.size, data.type, data.endian, data.samples, data.lines, region.r, region.SPDF)}))
        stopCluster(cl)
        
        
        # Name the layers
        names(region.ts) <- data.names
        
        # Set layers' dates
        data.dates <- as.Date(as.yearmon(data.dates))
        data.dates[1:length(data.dates) %% 2 == 0] <- data.dates[1:length(data.dates) %% 2 == 0]  + 15
        region.ts <- setZ(region.ts, data.dates)
        stackDates <- getZ(region.ts)
        

        # Calculate
        #
        
        message("Processing dataset ...","\n",appendLF=FALSE)
        flush.console()
        
        #Run the selected method on parallel tasks
        beginCluster(nCores,type="SOCK") #number of cores to use
        cl <-getCluster()
        clusterExport(cl, list("stackDates", "PolyClassToRaster", "PolyTrend", "jarque.bera.test.pval", "bartlet.test.pval", "residuals.tests", "cubic.test", "quadratic.test"), envir=environment())
        rc.trends <- clusterR(region.ts, calc, args=list(fun=function(x) {PolyClassToRaster(x,stackDates)}, forceapply=TRUE))
        endCluster()
        #rc.trends <- calc(region.ts, fun=function(x) {PolyToRaster$Function(x,stackDates)}, forceapply=TRUE)
                
        names(rc.trends) <- c("TrendType", "Significance", "PolynomialDegree", "Slope", "Direction")
        
        # Report
        message("Writing in selected output directory..","\n",appendLF=FALSE)
        flush.console()
        
        output.files <- paste(output.path, format(data.date.start, format="%Y%b"), format(data.date.end, format="%Y%b"), "." , names(rc.trends), "." , "PolyTrends", sep="")
        writeRaster(rc.trends, filename=output.files, bylayer=TRUE, format=output.format, overwrite=TRUE)
        
        output.statfile <- paste(output.path, format(data.date.start, format="%Y%b"), format(data.date.end, format="%Y%b"), "." , "PolyTrends.txt", sep="")
        out<-capture.output(freq(rc.trends))
        cat(out,file=output.statfile,sep="\n")
        
        message("Done.","\n",appendLF=FALSE)
        flush.console()
        
}