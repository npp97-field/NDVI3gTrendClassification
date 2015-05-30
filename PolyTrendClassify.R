jarque.bera.test.pval <- function(res) {
        
        n <- length(res)
        m1 <- sum(res)/n
        m2 <- sum((res-m1)^2)/n
        m3 <- sum((res-m1)^3)/n
        m4 <- sum((res-m1)^4)/n
        b1 <- (m3/m2^(3/2))^2
        b2 <- (m4/m2^2)
        STATISTIC <- n*b1/6+n*(b2-3)^2/24
        pval <- 1 - pchisq(STATISTIC,df = 2)
        return(pval)
}

bartlet.test.pval <- function(res) {
        
        if (length(res) %% 2 == 0) {
                res1 <- res[1:as.integer(length(res)/2)]
                res2 <- res[as.integer((length(res)/2)+1):length(res)]
        } else {
                res1 <- res[1:as.integer(length(res)/2)]
                res2 <- res[as.integer(length(res)/2):length(res)]
        }
        
        pval <- bartlett.test(list(res1,res2))$p.value
        return(pval)
        
}

residuals.tests <- function(fit,alpha) {
        
        res <- residuals(fit)
        
        check1 <- jarque.bera.test.pval(res) > alpha
        check2 <- bartlet.test.pval(res) > alpha
        check3 <- round(t.test(res)$estimate,4) == 0
        
        if(!is.finite(check1)) check1=0
        if(!is.finite(check2)) check2=0
        if(!is.finite(check3)) check3=0
        
        validate.all <- FALSE
        
        if(check1 && check2 && check3) {
                validate.all <- TRUE
        } 
        
        return(validate.all)
        
}

cubic.test <- function(p.cubic) {
        
        a <- unname(coef(p.cubic)[4]) ## I(X^3)
        b <- unname(coef(p.cubic)[3]) ## I(X^2)
        c <- unname(coef(p.cubic)[2]) ## X
        d <- unname(coef(p.cubic)[1]) ## constant
        
        critical.point <- (b^2 - 3 * a * c) # > 0, not monotonic, with presence of a local maximum and a local minimum.
        
        x1.turn <- (-b + sqrt(b^2 - 3*a*c))/(3*a)
        x2.turn <- (-b - sqrt(b^2 - 3*a*c))/(3*a)
        y1.turn <- a*x1.turn^3 + b*x1.turn^2 + c*x1.turn + d
        y2.turn <- a*x2.turn^3 + b*x2.turn^2 + c*x2.turn + d
        
        if(!is.finite(x1.turn)) x1.turn=0
        if(!is.finite(x2.turn)) x2.turn=0
        if(!is.finite(y1.turn)) y1.turn=0
        if(!is.finite(y1.turn)) y2.turn=0
        
        cubic.minmax <- FALSE
        if(critical.point > 0 && y1.turn > 0 && y1.turn <= 1 && y2.turn > 0 && y2.turn <= 1) {
                cubic.minmax <- TRUE
        }
                
        return(cubic.minmax)
}

quadratic.test <- function(p.quadratic) {
        
        a <- unname(coef(p.quadratic)[3]) ## I(X^2)
        b <- unname(coef(p.quadratic)[2]) ## X
        c <- unname(coef(p.quadratic)[1]) ## constant
        
        y.turn <- c - (b^2/4*a)
        if(!is.finite(y.turn)) y.turn=0
        
        quadratic.minmax <- FALSE
        if(y.turn > 0 && y.turn <= 1) {
                quadratic.minmax <- TRUE
        }
        
        return(quadratic.minmax)
        
}

PolyTrend <- function(Y, alpha) {
        
        X <- c(1:length(Y))
        
        p1 <- lm(Y ~ X) ## Liner fit
        p2 <- lm(Y ~ X + I(X^2)) ## Quadratic fit
        p3 <- lm(Y ~ X + I(X^2) + I(X^3)) ## Cubic fit
        
        p1.pvalue <- coef(summary(p1))[2,4]
        p2.pvalue <- coef(summary(p2))[3,4]
        p3.pvalue <- coef(summary(p3))[4,4]
        
        
        # Default
        TrendType <- 0
        Significance <- -1
        PolynomialDegree <- 0
        Slope <- 0
        Direction <- 0
        
        # 
        p.AIC <- c()
        
        if(p1.pvalue < alpha && residuals.tests(p1,alpha) == TRUE)
        {
                p.AIC <- c("1" = AIC(p1), p.AIC) ## Add AIC value
        }
        
        if(p2.pvalue < alpha && residuals.tests(p2,alpha) == TRUE && quadratic.test(p2) == TRUE)
        {
                p.AIC <- c("2" = AIC(p2), p.AIC)
        }
        
        if(p3.pvalue < alpha && residuals.tests(p3,alpha) == TRUE  && cubic.test(p3) == TRUE)
        {
                p.AIC <- c("3" = AIC(p3), p.AIC)
        }        
        
        if(length(p.AIC) > 0) {
          
                Slope <- round(coef(p1)["X"], 3)
                Direction <- sign(Slope)
                
                # Select the best AIC
                bestAIC <- names(p.AIC)[which.min(p.AIC)]
                PolynomialDegree <- as.numeric(bestAIC)
  
                if(p1.pvalue < alpha) {
                        
                        Significance <- 1
                        TrendType <- PolynomialDegree
                        
                } else {
                        
                        TrendType <- -1
                }

        }

        return(c("TrendType"= TrendType, "Significance" = Significance, "PolynomialDegree" = PolynomialDegree, "Slope" = Slope, "Direction" = Direction))

}
    
        
        