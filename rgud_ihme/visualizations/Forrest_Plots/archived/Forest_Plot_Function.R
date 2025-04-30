generateForestPlot <- function(data, effect_size_col, variance_col, author_col, year_col, loc_id_col, xlims = c(-2, 3), ylims = c(-1, 54), xlab = "Fraction of health spending attributable to dementia") {
  library(metafor)
  library(data.table) # For data manipulation
  library(ggplot2)    # Assuming you might need it for additional customization
  
  # Ensure data is in the data.table format for efficiency
  if (!inherits(data, "data.table")) {
    data <- as.data.table(data)
  }
  
  # Calculate necessary variables
  data[, sample_size_AD := as.numeric(sample_size_AD)]
  data[, sample_size_control := as.numeric(sample_size_control)]
  data[, var_2 := (1/sample_size_AD) + (1/sample_size_control)]
  data[, slab := paste(get(author_col), get(year_col), get(loc_id_col), sep="  ")]
  
  # Fit random-effects model
  res <- rma(get(effect_size_col), get(variance_col), method="REML", data=data)
  
  # Generate forest plot
  forest(res, xlim=xlims, cex=0.75, ylim=ylims, addfit = FALSE,
         slab = data$slab,
         xlab = xlab, 
         mlab = "", 
         psize = 1, 
         header = "Author(s) and Year")
}

# Creating dummy data
dummy_data <- data.frame(
  Author = c("Smith", "Johnson", "Williams"),
  year_published = c(2019, 2020, 2021),
  ihme_loc_id = c("USA", "GBR", "CAN"),
  sample_size_AD = c(100, 150, 120),
  sample_size_control = c(200, 250, 220),
  At_Frac_total = c(0.2, 0.3, 0.25)
)

# Convert to data.table (optional here since the function checks and converts)
library(data.table)
dummy_data <- as.data.table(dummy_data)

generateForestPlot(data = dummy_data, 
                   effect_size_col = "At_Frac_total", 
                   variance_col = "var_2", 
                   author_col = "Author", 
                   year_col = "year_published", 
                   loc_id_col = "ihme_loc_id")
