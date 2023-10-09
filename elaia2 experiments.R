boxplot(elaia2$num_provider ~ elaia2$opioid)
boxplot(elaia2$max_pain_score14 ~ elaia2$opioid)
wilcox.test(max_pain_score14 ~ opioid, data = elaia2)
wilcox.test(num_provider ~ opioid, data = elaia2)
wilcox.test(num_provider ~ alert, data = elaia2)
t.test(elaia2$num_provider & elaia2$opioid == 0, elaia2$num_provider & elaia2$opioid == 1)
t.test(elaia2$num_provider & elaia2$alert == 0, elaia2$num_provider & elaia2$alert == 1)
t.test(elaia2$num_provider & elaia2$ventorder == 0, elaia2$num_provider & elaia2$ventorder == 1)
t.test(elaia2$num_provider & elaia2$transfuserbc == 0, elaia2$num_provider & elaia2$transfuserbc == 1)
t.test(elaia2$los_since_alert & elaia2$ventorder == 0, elaia2$los_since_alert & elaia2$ventorder == 1)
t.test(elaia2$num_provider & elaia2$transfuserbc == 0, elaia2$num_provider & elaia2$transfuserbc == 1)
t.test(elaia2$num_provider & elaia2$sex == 0, elaia2$num_provider & elaia2$sex == 1)


# Load necessary libraries
library(ggplot2)
library(ggsignif)

# Create boxplot
boxplot <- ggplot(elaia2, aes(x=opioid, y=max_pain_score14)) + geom_boxplot()

# Add significance bars
boxplot <- boxplot + geom_signif(comparisons = list(c(elaia2$num_provider & elaia2$opioid == 1, elaia2$num_provider & elaia2$opioid == 0)), map_signif_level=TRUE)

# Print boxplot with significance bars
print(boxplot)