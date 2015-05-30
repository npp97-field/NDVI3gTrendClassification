
PolyClassToRaster <- function(x, stackDates) {
        
        if(any(is.na(x))) { ## filter NA
                
                      return(c(NA, NA, NA, NA, NA))
                
        } else {
                
                ndvi3g.ts <-zoo(as.vector(x), stackDates)
                
                if (any(as.vector(rollapply(ndvi3g.ts, 24, mean, by = 24)) < 0.1)) { ## check for long period without veg
                        return(c(NA, NA, NA, NA, NA))
                } else {
                        
                        ndvi3g.ts <- aggregate(ndvi3g.ts, as.yearmon, mean)
                        by.years <- split( as.vector(ndvi3g.ts), as.vector(format(time(ndvi3g.ts), "%y")) )
                        ndvi3g.ts <- as.vector(sapply(by.years, function(x) mean(sort(x, decreasing = TRUE)[1:4])))
                        PT <- PolyTrend(Y = ndvi3g.ts, alpha = 0.05)
                        return(as.vector(PT))                        
                        
                }
                
        } ## end na check
        
} ## end

