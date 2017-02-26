# Numerai

library(ggplot2)
library(reshape2)

tr <- read.csv("Training 20170108.csv", stringsAsFactors = F)

# Get correlation matrix
crr <- cor(tr)

# Keep the lower triangle of the matrix
crr[lower.tri(crr)] <- NA
corrDF <- melt(crr)

# Plot the matrix
ggplot(data = corrDF, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
