# Base R Graphics Charts

# Histogram
data <- rnorm(100)
hist(data, col="lightblue", border="black", main="Histogram", xlab="Values", ylab="Frequency", 
     breaks=10, las=1, font.main=2, cex.lab=1.2)

# Vertical Bar Chart
categories <- c("A", "B", "C", "D")
values <- c(4, 7, 9, 3)
barplot(values, names.arg=categories, col="lightblue", main="Vertical Bar Chart", 
        ylab="Count", xlab="Categories", border="black", las=1, cex.names=1.2)

# Horizontal Bar Chart
barplot(values, names.arg=categories, col="lightblue", horiz=TRUE, main="Horizontal Bar Chart", 
        xlab="Count", border="black", las=1, cex.names=1.2)

# Pie Chart
pie(values, labels=categories, col=c("lightblue", "lightgreen", "lightpink", "lightyellow"), 
    main="Pie Chart", radius=1, border="black")

# Boxplot
boxplot_data <- list(Group1=rnorm(50), Group2=rnorm(50, mean=3), Group3=rnorm(50, mean=6))
boxplot(boxplot_data, col=c("lightblue", "lightgreen", "lightpink"), main="Boxplot", 
        ylab="Values", border="darkblue", las=1, font.lab=2, cex.lab=1.2)

# Scatterplot
x <- rnorm(50)
y <- x + rnorm(50, sd=0.5)
plot(x, y, main="Scatterplot", xlab="X Axis", ylab="Y Axis", pch=19, col="darkblue", 
     cex=1.5, font.main=2, cex.lab=1.2, col.axis="black", las=1)

# Export Base R Charts
pdf("base_histogram.pdf")
hist(data)
dev.off()

jpeg("base_histogram.jpg")
hist(data)
dev.off()

svg("base_histogram.svg")
hist(data)
dev.off()

tiff("base_histogram.tiff")
hist(data)
dev.off()

bmp("base_histogram.bmp")
hist(data)
dev.off()


# ggplot2 Charts

library(ggplot2)

# Histogram (ggplot2)
data <- data.frame(values = rnorm(100))
ggplot(data, aes(x=values)) +
  geom_histogram(binwidth=0.5, fill="lightblue", color="black") +
  labs(title="Histogram", x="Values", y="Frequency") +
  theme_minimal() +
  theme(text = element_text(size=12, face="plain"))

# Vertical Bar Chart (ggplot2)
bar_data <- data.frame(categories=c("A", "B", "C", "D"), values=c(4, 7, 9, 3))
ggplot(bar_data, aes(x=categories, y=values)) +
  geom_bar(stat="identity", fill="lightblue", color="black") +
  labs(title="Vertical Bar Chart", x="Categories", y="Count") +
  theme_classic() +
  theme(text = element_text(size=12, face="italic"))

# Horizontal Bar Chart (ggplot2)
ggplot(bar_data, aes(x=values, y=categories)) +
  geom_bar(stat="identity", fill="lightblue", color="black") +
  labs(title="Horizontal Bar Chart", x="Count", y="Categories") +
  theme_light() +
  theme(axis.text=element_text(size=10, face="bold"))

# Corrected Pie Chart (ggplot2)
ggplot(bar_data, aes(x="", y=values, fill=categories)) +
  geom_bar(stat="identity", color="black") +
  coord_polar("y") +
  labs(title="Pie Chart") +
  scale_fill_manual(values=c("lightblue", "lightgreen", "lightpink", "lightyellow")) +  # Custom colors
  theme_void() +
  theme(legend.position="bottom")

ggplot(boxplot_data, aes(x=Group, y=Values, fill=Group)) +
  geom_boxplot(outlier.shape=19, outlier.colour="darkred") +  # Customize outliers if desired
  labs(title="Boxplot", x="Group", y="Values") +  # Add axis labels
  scale_fill_manual(values=c("lightblue", "lightgreen", "lightpink", "lightyellow")) +  # Custom colors
  theme_bw() +  # Use a black and white theme
  theme(text = element_text(size=12, family="Arial", face="bold")) 

# Scatterplot (ggplot2)
scatter_data <- data.frame(x = rnorm(50), y = rnorm(50))
ggplot(scatter_data, aes(x=x, y=y)) +
  geom_point(color="blue", size=3) +
  labs(title="Scatterplot", x="X Axis", y="Y Axis") +
  theme_minimal() +
  theme(text = element_text(size=12, face="bold", color="darkblue"))

# Export ggplot2 Charts
ggsave("ggplot_histogram.pdf", plot=last_plot(), width=5, height=5)
ggsave("ggplot_histogram.jpg", plot=last_plot(), width=5, height=5, dpi=300)
ggsave("ggplot_histogram.svg", plot=last_plot(), width=5, height=5)
ggsave("ggplot_histogram.tiff", plot=last_plot(), width=5, height=5, dpi=300)
ggsave("ggplot_histogram.bmp", plot=last_plot(), width=5, height=5, dpi=300)
