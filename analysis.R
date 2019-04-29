library(readr)
library(corrplot)

# https://hansenjohnson.org/post/sync-github-repository-with-existing-r-project/

plot_residuals <- function(residuals, file_prefix, method="circle", residuals.limit, title) {
  print(corrplot(residuals, 
                 is.cor = FALSE, 
                 method=method, 
                 tl.col="black", 
                 cl.lim = residuals.limit,
                 tl.srt=45, 
                 addgrid.col="black", 
                 bg="black",
                 number.cex = .6,
                 cl.pos="b", 
                 cl.length = 3, 
                 title=title, 
                 mar=c(0,0,4.5,0)))
  dev.copy(png, paste('plots', paste(file_prefix, '_residuals_plot_by_', method , '.png', sep=''), sep="/"))
  dev.off()
}

plot_contributions <- function(contributions, file_prefix, method="circle", contributions.limit, title) {
  print(corrplot(contributions, 
                 is.cor = FALSE, 
                 method=method, 
                 tl.col="black", 
                 cl.lim = contributions.limit,
                 tl.srt=45, 
                 addgrid.col="black", 
                 bg="black", 
                 number.cex = .6,
                 cl.pos="b", 
                 cl.length = 3, 
                 title=title, 
                 mar=c(0,0,4.5,0)))
  dev.copy(png, paste('plots', paste(file_prefix, '_contributions_plot_by_', method, '.png', sep=''), sep="/"))
  dev.off()
}

analyze_trail_access <- function(file_name, 
                                 base_title, 
                                 residuals.limit = c(-25, 25), 
                                 contributions.limit = c(0,100), 
                                 housing_types = c('APT', 'CDO','COR', 'DUP', 'MOB', 'PCL', 'PUB', 'HSE')) {
  
  file_name_without_ext <- tools::file_path_sans_ext(file_name)

  d1 <- read.csv(file=paste('data', file_name, sep="/"), header=TRUE, sep=",", stringsAsFactors=FALSE)
  m1 <- as.matrix(d1[,-1])
  rownames(m1) <- d1[,1]
  colnames(m1) <- c("0 to 0.25 mile", "0.25 to 0.5 mile", "0.50 to 1 mile")
  
  m1 <- m1[which(rownames(m1) %in% housing_types),]
  
  cht1 <- chisq.test(m1)
  
  print(cht1)
  
  # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
  # http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
  
  #cht1$observed
  #round(cht1$expected,2)
  
  print('residuals')
  cht1$residuals <- round(cht1$residuals, 3)
  
  print(cht1$residuals)

  
  title <- c("Residuals for ", base_title, sep="")
  plot_residuals(residuals = cht1$residuals, method="circle", file_prefix=file_name_without_ext, residuals.limit = residuals.limit, title=title)
  plot_residuals(residuals = cht1$residuals, method="number", file_prefix=file_name_without_ext, residuals.limit = residuals.limit, title=title)
  
  # print(corrplot(cht1$residuals, is.cor = FALSE, method="circle", tl.col="black", cl.lim = residual.lim,
  #          tl.srt=45, addgrid.col="black", bg="black", cl.pos="b", cl.length = 3, title=title, mar=c(0,0,4.5,0)))
  # dev.copy(png,paste(file_name_without_ext, '_residuals_plot.png', sep=''))
  # dev.off()
  
  
  
  # Contibution in percentage (%)
  cht1$contributions <- 100*cht1$residuals^2/cht1$statistic
  cht1$contributions <- round(cht1$contributions, 3)
  
  print('contributions')
  print(cht1$contributions)
  
  title <- c("Contributions for ", base_title, sep="")
  plot_contributions(contributions = cht1$contributions, method="circle", file_prefix=file_name_without_ext, contributions.limit = contributions.limit, title=title)
  plot_contributions(contributions = cht1$contributions, method="number", file_prefix=file_name_without_ext, contributions.limit = contributions.limit, title=title)
  
  # print(corrplot(cht1$contributions, is.cor = FALSE, method="circle", tl.col="black", cl.lim = contribution.lim,
  #          tl.srt=45, addgrid.col="black", bg="black", cl.pos="b", cl.length = 3, title=title, mar=c(0,0,4.5,0)))
  # dev.copy(png,paste(file_name_without_ext, '_contributions_plot.png', sep=''))
  # dev.off()
  
  
  
  return(cht1)
}

future_and_existing <- analyze_trail_access(file_name="existing_and_future_greenway.csv", base_title = "Existing and Future Greenway")
existing <- analyze_trail_access(file_name="existing_greenway.csv", base_title = "Existing Greenway")

# determine the devations from existing to future plans"
title <- "Housing Unit Count Changes between Future and Existng Plans"
print(corrplot(future_and_existing$observed - existing$observed, is.cor = FALSE, method="number", tl.col="black", col = cm.colors(100), bg="black", cl.lim = c(-800,800),
               tl.srt=45, addgrid.col="black", cl.pos="b", cl.length = 3, title=title, mar=c(0,0,4.5,0)))
dev.copy(png,'future_and_existing_changes_counts_by_number.png')
dev.off()

title <- "" # "Housing Unit Count Changes between Future and Existng Plans"
print(corrplot(future_and_existing$observed - existing$observed, is.cor = FALSE, method="circle", tl.col="black", col = cm.colors(100), bg="black", cl.lim = c(-800,800),
               tl.srt=45, addgrid.col="black", cl.pos="b", cl.length = 3, title=title, mar=c(0,0,4.5,0)))
dev.copy(png,'future_and_existing_changes_counts_by_circle.png')
dev.off()


norg <- analyze_trail_access(file_name="norg.csv", base_title = "North Oconee River Greenway")
firefly <- analyze_trail_access(file_name="firefly.csv", base_title = "Firefly Trail")
pulaski <- analyze_trail_access(file_name="pulaski.csv", base_title = "Pulaski Heights Trail")
trlcrk <- analyze_trail_access(file_name="trlcrk.csv", base_title = "Trail Creek Greenway", housing_types=c('APT', 'CDO', 'DUP', 'MOB', 'PCL', 'PUB', 'HSE'))
mext <- analyze_trail_access(file_name="mext.csv", base_title = "Milledge Extension Trail", housing_types =c('APT','CDO', 'DUP', 'PCL', 'HSE'))

