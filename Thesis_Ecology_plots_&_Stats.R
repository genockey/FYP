# Set the working directory to the folder containing the CSV files
setwd("path/to/folder/containing/csv/files")

### Housekeeping - setting up files, making sure everything is in order, and loading packages 

landtype <- read.csv("landtype.csv")
plants <- read.csv("Total_Dublin.csv")
totalplants <- read.csv("Total_Dublin_Sites.csv")


# libraries
library(tidyverse)
library(vegan)
library(ggplot2)

View(plants)
View(landtype)

## TELL R THAT "SITE_TYPE" IS FACTOR DATA
## tell R that "preference" is factor data

SITE_TYPE <-as.factor(landtype$site_type)

## How speciose is each of the sites? 

specnumber() will tell us the number of species within each sample. 
We can then run an analysis of variance to ask if mean species richness is significantly different across sites.

view(plants)
sppr <- specnumber(plants)

shapiro.test(sppr)

### ANOVA test to investigate whether species richness is different
sppr_aov <- aov(sppr ~ site_type, data = landtype)
y <- summary(sppr_aov)
print(y)

## Mann Whitney U test
view(sppr)
wilcox.test(sppr ~ site_type, data = landtype, exact = FALSE)



# Create a boxplot
boxplot(sppr ~ site_type, data = landtype, main = "Boxplot of SPPR by Site Type", xlab = "Level of Urbanisation", ylab = "Species Richness")

library(ggplot2)
ggplot(data = landtype, aes(x = site_type, y = sppr, fill = site_type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gold1", "palegreen4")) +
  ggtitle("Boxplot of Species Richness at each level of urbanisation") +
  xlab("Level of urbanisation") +
  ylab("Species Richness") +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12))

## Species Diversity

# Calculate the Shannon diversity of each sample
shannondiv <- diversity(plants[,2:110], index = "shannon")
# Print the results
print(shannondiv)

shapiro.test(shannondiv)
var(shannondiv)
View(shannon)

## Mann Whitney U test
wilcox.test(shannondiv ~ site_type, data = landtype, exact = FALSE)

## Welch's T-test
t.test(shannondiv ~ site_type, data = landtype, var.equal = FALSE)


## Log transform
shannonlog <- log10(shannondiv)
shapiro.test(shannonlog)

wilcox.test(shannonlog ~ site_type, data = landtype, exact = FALSE)

sppdiv_aov <- aov(shannondiv ~ site_type, data = landtype)
summary(sppdiv_aov)

## Column Plot 

library(ggplot2)
library(dplyr)
library(tidyr)

# Calculate mean and standard error of Shannon diversity by site_type
div_summary <- landtype %>%
  group_by(site_type) %>%
  summarize(mean_div = mean(shannondiv), se_div = sd(shannondiv)/sqrt(n()))

# Create the column plot
library(ggplot2)

# Calculate mean and standard error of diversity for each site type
means <- aggregate(shannondiv ~ site_type, data = landtype, mean)
se <- aggregate(shannondiv ~ site_type, data = landtype, function(x) sd(x)/sqrt(length(x)))

# Create the plot
p <- ggplot(data = means, aes(x = site_type, y = shannondiv, fill = site_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("gold1", "palegreen4")) +
  ggtitle("Shannon Diversity") +
  xlab("Level of Urbanisation") +
  ylab("Shannon Diversity Index") +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 18))

# Add error bars and text labels for mean values
p <- p + geom_errorbar(aes(ymin = shannondiv - se$shannondiv, ymax = shannondiv + se$shannondiv),
                       width = 0.2, position = position_dodge(0.9), colour = "gray20") +
  geom_text(aes(label = paste0("mean = ", round(shannondiv, 2))), position = position_nudge(0.3), vjust = -0.5)

# Show the plot
p

## Boxplot 

ggplot(data = landtype, aes(x = site_type, y = shannondiv, fill = site_type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gold1", "palegreen4")) +
  ggtitle("Boxplot of Shannon Diversity at each level of urbanisation") +
  xlab("Level of urbanisation") +
  ylab("Shannon Diversity") +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12))

## Abundance: 
plants_subset <- plants[, -1]

# Calculate the row sums
rowsums <- rowSums(plants_subset)

# Print the row sums
print(rowsums)

shapiro.test(rowsums)
## Mann Whitney U test
wilcox.test(rowsums ~ site_type, data = landtype, exact = FALSE)

## Boxplot

ggplot(data = landtype, aes(x = site_type, y = rowsums, fill = site_type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gold1", "palegreen4")) +
  ggtitle("Boxplot of Species Abundance at each level of urbanisation") +
  xlab("Level of urbanisation") +
  ylab("Species Abundance") +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12))



## individual sites rather than levels of urbanisation 
# Create a boxplot

# Load required packages
library(tidyverse)

# Read in the data
my_data <- read.csv("Total_Dublin_Sites.csv")


library(vegan)
library(ggplot2)

# Read in data

my_data <- read.csv("plottingdata.csv")
View(my_data)
View(plants)

# Convert site_ID to a factor
my_data$site_ID <- as.factor(my_data$site_ID)

# Convert site_type to a factor
my_data$site_type <- as.factor(my_data$site_type)

## Convert sr to numeric 
my_data$sr <- as.numeric(my_data$sr)

## Convert abundance to numeric
my_data$abun <- as.numeric(my_data$abun)

library(ggplot2)

## species richness boxplots

ggplot(my_data, aes(x = site_ID, y = sr, fill = site_type)) +
  geom_boxplot(width=0.7, position=position_dodge(width=0.8)) +
  labs(x = "Site ID", y = "Species Richness", fill = "") +
  scale_fill_manual(values = c("gold1", "palegreen4"), labels = c("High Urbanisation", "Medium Urbanisation")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(),
        panel.spacing = unit(2, "cm"),
        plot.background = element_rect(fill = "white", color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = unit(c(1, 2, 1, 1), "cm"),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "lines"),
        legend.background = element_rect(color = "black"),
        legend.margin = margin(5, 5, 5, 5),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.justification = "center")


## abundance boxplots

view(my_data$abun)


## Statistical analysis

summary(my_data$sr)
summary(my_data$abun)

sd(my_data$sr)
sd(my_data$abun)

var(my_data$sr)
var(my_data$abun)

## Look at the data visually 
# Create a histogram of my_data$sr
hist(my_data$sr, 
     main = "Histogram of Species Richness", # add a main title
     xlab = "Species Richness", # add a x-axis label
     col = "palegreen4", # set the fill color
     border = "white" # set the border color
)

## Create a Histogram of my_data$abun
# Create a histogram of my_data$sr
hist(my_data$abun, 
     main = "Histogram of Species Richness", # add a main title
     xlab = "Species Richness", # add a x-axis label
     col = "palegreen4", # set the fill color
     border = "white" # set the border color
)

## Assessing the normality of the dataset
# Create a QQ plot of my_data$sr
qqnorm(my_data$sr, 
       main = "QQ plot of Species Richness", # add a main title
       xlab = "Theoretical Quantiles", # add a x-axis label
       ylab = "Sample Quantiles", # add a y-axis label
       col = "palegreen4" # set the line color
)
qqline(my_data$sr) # add a reference line to the plot

qqnorm(my_data$abun, 
       main = "QQ plot of Abundance", # add a main title
       xlab = "Theoretical Quantiles", # add a x-axis label
       ylab = "Sample Quantiles", # add a y-axis label
       col = "palegreen4" # set the line color
)
qqline(my_data$abun)

## As most points do not fall along this reference line, we cannot assume normality
## visual inspections like the qqplot are sometimes unreliable so it is important that we also use significance tests
## H0 in these tests is that the distribution is normal, if the p-value is significant, then the data are non-normal

shapiro.test(my_data$sr)
shapiro.test(my_data$abun)

## log transform to run parametric tests 
logsr <- log10(my_data$sr)
shapiro.test(logsr)

## ANOVA test to see if there is any significant differences in mean at each site 
# Perform an ANOVA on my_data$sr
anova_result <- aov(sr ~ site_ID, data = my_data)

# Print the ANOVA table
summary(anova_result)

## Abundance
## log transform to run parametric tests 
logabun <- log10(my_data$abun)
shapiro.test(logabun)

# ANOVA test to see if there is any significant differences in mean abundance at each site 
# Perform an ANOVA on my_data$sr
anova_result <- aov(abun ~ site_ID, data = my_data)

# Print the ANOVA table
summary(anova_result)


# Perform the Levene's test
levene_result <- leveneTest(sr ~ site_ID, data = my_data)

view(plants)

## bray curtis dissimilarity -> beta diversity

set the site column as the rownames
rownames(plants) <- plants$x
plants$x <- NULL

# calculate the Bray-Curtis dissimilarity
bray_dist <- vegdist(plants, method = "bray")

view(plants)

# view the data
view(plants)

# convert data to numeric format
plants_numeric <- apply(plants[, -1], 2, function(x) as.numeric(as.character(x)))

# set rownames to first column and remove first column
rownames(plants_numeric) <- plants[, 1]
plants_numeric <- plants_numeric[, -1]

# calculate Bray-Curtis dissimilarity
bray_dist <- vegdist(plants_numeric, method = "bray")

bray_dist

## visualising this 

library(ggplot2)
library(reshape2)

# Convert the distance matrix to a data frame
bray_dist_df <- as.data.frame(as.matrix(bray_dist))

# Convert the data frame to a long format
bray_dist_melt <- melt(bray_dist_df)

library(ggplot2)
library(viridis)

# Calculate the Bray-Curtis dissimilarity
bray_dist <- vegdist(plants, method = "bray")

# Convert the distance matrix to a data frame
bray_df <- as.data.frame(as.matrix(bray_dist))
bray_df$Plant1 <- rownames(bray_df)
bray_df_long <- reshape2::melt(bray_df, id.vars = "Plant1", variable.name = "Plant2", value.name = "Distance")


# Convert Plant1 and Plant2 to factor variables with the same levels
bray_df_long$Plant1 <- factor(bray_df_long$Plant1, levels = unique(bray_df_long$Plant1))
bray_df_long$Plant2 <- factor(bray_df_long$Plant2, levels = unique(bray_df_long$Plant1))

# Sort the data frame by Plant1 and Plant2
bray_df_long <- bray_df_long[order(bray_df_long$Plant1, bray_df_long$Plant2), ]

# Create the heatmap with ggplot2
ggplot(data = bray_df_long, aes(x = Plant1, y = Plant2, fill = Distance)) + 
  geom_tile() +
  scale_fill_viridis(name = "Bray-Curtis Dissimilarity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Statistical Analysis 

# Create grouping variable
groups <- ifelse(row.names(bray_df) %in% c("Phibsboro", "Ringsend", "Townsend", "Bridgefoot"), "High urbanisation", "Medium urbanisation")
library(vegan)

# Permutation test

# Load required packages
library(vegan)
library(ggplot2)

# Check homogeneity of variance
betadisper(bray_dist, groups)

# Visualize distribution of distances for each group
ggplot(bray_df_long, aes(x = Distance, fill = groups)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  facet_wrap(~ groups)

# Run PERMANOVA test
perm_test <- adonis(bray_dist ~ groups, permutations = 999)

# Access F-statistic value
F_value <- perm_test$aov.tab$F[1]
F_value

perm_test <- adonis2(bray_dist ~ groups, permutations = 999)
perm_test
summary(perm_test)
summary(perm_test$aov.tab)


# Extract F-statistic and p-value
F_statistic <- perm_test$aov.tab$`F-value`[1]
p_value <- perm_test$aov.tab$`Pr(>F)`[1]

# Print results
cat("F-statistic:", F_statistic, "\n")
cat("p-value:", p_value, "\n")
