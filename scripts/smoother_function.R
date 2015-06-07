# smooth for contour line representations 

smooth_var <- function(dta,  group_vars, smooth_var, smooth_par){
  
  dta_ss <- dta[, c("age", "year", group_vars, smooth_var)]
  
  smooth_subfn <- function(xx){
    yy <- xx %>% spread(key=age, value=var_to_smooth)
    years <- yy$year
    yy$year <- NULL
    yy <- as.matrix(yy)
    rownames(yy) <- years
    
    yy[is.infinite(yy) & yy < 0] <- min(yy[is.finite(yy)]) # correct for infinities
    yy[is.infinite(yy) & yy > 0] <- max(yy[is.finite(yy)])
    zz <- as.matrix(blur(as.im(yy), sigma=smooth_par))  
    rownames(zz) <- rownames(yy)
    colnames(zz) <- colnames(yy)
    
    
    zz <- as.data.frame(zz)
    zz$year <- rownames(zz)
    zz <- zz %>% gather(key=age, value=smoothed_var, -year)
    zz$age <- zz$age %>%
      str_replace("X", "") %>%
      as.character %>%
      as.numeric
    
    return(zz)
  }
  
  manage_smooth_fn <- function(x){
    x <- x[!is.na(x[,smooth_var]),] 
    
    dta_groups <- x[1, group_vars]
    dta_to_smooth <- x[, c("age", "year", smooth_var)]
    names(dta_to_smooth)[3] <- "var_to_smooth"
    
    dta_smoothed <- smooth_subfn(dta_to_smooth)
    
    dta_smoothed <- data.frame(dta_groups, dta_smoothed)
    
    return(dta_smoothed)
  }
  
  smoothed_df <- ddply(dta_ss, group_vars, manage_smooth_fn)
  names(smoothed_df)[length(names(smoothed_df))] <- smooth_var
  smoothed_df$year <- as.numeric(smoothed_df$year)
  smoothed_df <- smoothed_df %>% tbl_df
  return(smoothed_df)
}