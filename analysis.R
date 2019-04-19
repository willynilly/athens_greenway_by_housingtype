library(readr)
library(corrplot)

# https://hansenjohnson.org/post/sync-github-repository-with-existing-r-project/

analyze_trail_access <- function(file_name, base_title) {
  file_name_without_ext <- tools::file_path_sans_ext(file_name)

  d1 <- read.csv(file=file_name, header=TRUE, sep=",", stringsAsFactors=FALSE)
  m1 <- as.matrix(d1[,-1])
  rownames(m1) <- d1[,1]
  colnames(m1) <- c("0 to 0.25 mile", "0.25 to 0.5 mile", "0.50 to 1 mile")
  
  cht1 <- chisq.test(m1)
  
  print(cht1)
  
  # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
  # http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
  cht1$observed
  round(cht1$expected,2)
  round(cht1$residuals,3)
  
  title <- c("Residuals for ", base_title, sep="")
  print(corrplot(cht1$residuals, is.cor = FALSE, method="circle", tl.col="black", cl.lim = c(-25,25),
           tl.srt=45, addgrid.col="black", cl.pos="b", cl.length = 3, title=title, mar=c(0,0,4.5,0)))
  dev.copy(png,paste(file_name_without_ext, '_residuals_plot.png', sep=''))
  dev.off()
  
  
  # Contibution in percentage (%)
  cht1$contributions <- 100*cht1$residuals^2/cht1$statistic
  cht1$contributions <- round(cht1$contributions, 3)
  
  
  title <- c("Contributions for ", base_title, sep="")
  print(corrplot(cht1$contributions, is.cor = FALSE, method="circle", tl.col="black", cl.lim = c(0,50),
           tl.srt=45, addgrid.col="black", cl.pos="b", cl.length = 3, title=title, mar=c(0,0,4.5,0)))
  dev.copy(png,paste(file_name_without_ext, '_contributions_plot.png', sep=''))
  dev.off()
  
}

analyze_trail_access(file_name="existing_and_future_greenway.csv", base_title = "Existing and Future Greenway")
analyze_trail_access(file_name="existing_greenway.csv", base_title = "Existing Greenway")


