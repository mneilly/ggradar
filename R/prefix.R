# Based on existing examples around the web

# ============================================================
# Generic unit prefix function
# ============================================================

# nums:    a vector of numbers to generate unit prefixes for
# names:   a vector of names for the chosen type of prefix
# syms:    a vector of symbols for each prefix
# breaks:  the break points representing the low value for each prefix range
# pfnums:  the resulting numbers with unit prefixes

prefix.generic <- function(nums, units, names, syms, breaks, forcenum=FALSE) {
  
  pfnums <- c()
  
  for (num in nums) {

    if (forcenum)
        num <- as.numeric(num)
    
    # Make sure it is a valid number
    if (is.numeric(num) & !is.na(num)) {
      
      # Get range the number falls into
      idx    <- findInterval(abs(num), breaks)
      idx    <- ifelse(idx, idx, 1)
      
      # Scale the number and append the prefix
      pfnum  <- paste0(paste0(num/breaks[idx], syms[idx]),units)
      pfnums <- append(pfnums, pfnum)      
      
    } else {
      # Not numeric! Just give it back.
      pfnum  <- as.character(num)
      pfnums <- append(pfnums, pfnum)
    }
  }
  pfnums
}

# ============================================================
# Binary unit prefix functions
# ============================================================

# Setup prefix names, symbols and breakpoints for prefix.generic

prefix.iec.binary <- function(nums, units="", forcenum=FALSE) {
  names   <- c("","kibi","mebi","gibi","tebi","pebi","exbi","zebi","yobi")
  
  syms    <- c("","Ki","Mi","Gi","Ti","Pi","Ei","Zi","Yi")
  
  breaks <- c(1,1024,1024^2,1024^3,1024^4,1024^5,1024^6,1024^7,1024^8)
  
  prefix.generic(nums, units, names, syms, breaks, forcenum)
}

prefix.jedec.binary <- function(nums, units="", forcenum=FALSE) {
  names   <- c("","kilo","mega","giga","tera","peta","exa","zeta","yotta")
  
  syms    <- c("","K","M","G","T","P","E","Z","Y")
  
  breaks <- c(1,1024,1024^2,1024^3,1024^4,1024^5,1024^6,1024^7,1024^8)
  
  prefix.generic(nums, units, names, syms, breaks, forcenum)
}

# ============================================================
# SI unit prefix function
# ============================================================

# Setup prefix names, symbols and breakpoints for prefix.generic

prefix.si.decimal <- function(nums, units="", forcenum=FALSE) {
  names <- c("yocto","zepto","atto","femto","pico",
             "nano","micro","milli","centi","deci",
             "",
             "deca","hecto","kilo","mega","giga","tera",
             "peta","exa","zetta","yotta")
  
  syms  <- c("y","z","a","f","p","n","μ","m","c","d",
             "",
             "da","h","k","M","G","T","P","E","Z","Y")
  
  breaks <- c(1e-24,1e-21,1e-18,1e-15,1e-12,1e-09,
              1e-06,1e-03,1e-02,1e-01,
              1e+00,
              1e+01,1e+02,1e+03,1e+06,1e+09,1e+12,
              1e+15,1e+18,1e+21,1e+24)
  
  prefix.generic(nums, units, names, syms, breaks, forcenum)
}

