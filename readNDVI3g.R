readNDVI3g <- function(data.file,data.size, data.type, data.endian, data.samples, data.lines, region.r, region.SPDF) {
        
        # Open and read the first found file
        f.in <- file(data.file,'rb')
        X <- readBin(f.in, integer(), n=data.size, size=data.type, endian=data.endian)
        close(f.in)
        
        # Put the grid values in matrix
        X <- matrix(X, nrow=data.samples, ncol=data.lines)
        
        # Define raster template 
        r.ext <- extent(c(-180, 180, -90 , 90))
        r.template <- raster(nrows = data.samples, ncols = data.lines, ext = r.ext)
        
        # Insert the matrix values in template
        r <- raster(X, template = r.template)
        
        # Set NA values and scale according to GIMMS 3g specification
        r[r[] == -10000] <- NA ## Water
        r[r[] == -5000] <- NA ## No data
        r <- r / 10000 ## Scale
        
        # If only to crop
        # r.crop <- crop(r,region.SPDF)
        
        # Mask with ME region and crop to ME
        r <- mask(r,region.r)
        r <- crop(r,region.SPDF)
}