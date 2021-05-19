#Code authored by C. Nagle, Iowa State University
#Updated on 2-5-21
#Nearly all data analysis for this paper will involve counts, tallies, etc.

#Read in the data and load required packages
data <- read.csv("Longitudinal Study Coding Book No Merino Lasagabaster.csv", row.names = NULL)
data$Length.Bin <- factor(data$Length.Bin, levels = c("0-4", "4-8", "8-12", "12-24", "24+"))
data$Participant.Age.Bin <- factor(data$Participant.Age.Bin, levels = c("Child", "Adolescent", "Adult"))
data$Participant.Age.Bin2 <- factor(data$Participant.Age.Bin2, levels = c("Younger", "Adult"))
data.pruned <- subset(data, Exclude.All.Multiple.Entries == "No")
data.age <- subset(data, Exclude.Save.Age == "No")
data.context <- subset(data, Exclude.Save.Context == "No")
data.agecontext <- subset(data, Exclude.Save.Age.Context == "No")
data.SPConstruct <- subset(data, Exclude.Save.SP.Construct == "No")
data.SPTask <- subset(data, Exclude.Save.SP.Task == "No")
data.SP <- subset(data, Exclude.Save.SP == "No")
library(dplyr)
library(ggplot2)

#Longitudinal Publication Trends ----
plot.studiesperyear <- ggplot(data.pruned, aes(Year)) +
  geom_bar(stat = "count", color = "black", fill = "white") +
  theme_bw() +
  xlab("Publication Year") +
  ylab("Count") +
  scale_x_continuous(breaks = c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
plot.studiesperyear
ggsave("Publications over time.png", plot = plot.studiesperyear, width = 5, height = 3, dpi = 300)

#General Trends ----

#Longitudinal characteristics
data.pruned.noDM84 <- data.pruned[-3, ]
mean(data.pruned.noDM84$Length.Months)
#Length bins
Length <- table(data.pruned$Length.Bin)
Length
#Data points
Points <- table(data.pruned$Data.Points)
Points
#Length in months
length.points.plot <- ggplot(data.pruned, aes(x = Length.Months, y = Data.Points, color = L2)) +
  geom_point(position = position_dodge(width = 2), alpha = 0.5, size = 3) +
  theme_bw() +
  labs(x = "Study Length in Months", y = "Study Data Points") +
  guides(size = F) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8)) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 16, 20, 24, 30), limits = c(0, 30)) 
length.points.plot
ggsave("length by data points.jpg", plot = length.points.plot, width = 5, height = 4, dpi = 300)

#Sample characteristics
sum(data.pruned$N.Speakers)
Age <- table(data.age$Participant.Age.Bin)
Age
Context <- table(data.context$Expanded.Context)
Context
L2 <- table(data.pruned$L2)
L2

#Analytical characteristics
SPConstruct <- table(data.SPConstruct$SP.Construct)
SPConstruct
ConstructxType <- table(data.SP$SP.Construct, data.SP$SP.Task)
ConstructxType

#Longitudinal by Participant Sample (Age, Context, L2) ----

#Study Length
plot.lengthbypop <- ggplot(data.agecontext, aes(Length.Bin)) + 
  geom_bar(stat = "count", color = "black", fill = "white", position = "dodge") +
  theme_bw() +
  xlab("Study Length") +
  ylab("Count") +
  facet_grid(Context ~ Participant.Age.Bin2) +
  theme(strip.background = element_blank()) +
  scale_y_continuous(limits = c(0, 10), breaks = c(2, 4, 6, 8)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = .5))
plot.lengthbypop

#Study Data Points
data.agecontext$Data.Points <- as.factor(data.agecontext$Data.Points)
plot.pointsbypop <- ggplot(data.agecontext, aes(Data.Points)) + 
  geom_bar(stat = "count", color = "black", fill = "white", position = "dodge") +
  theme_bw() +
  xlab("Study Data Points") +
  ylab("Count") +
  facet_grid(Participant.Age.Bin2 ~ Context) +
  theme(strip.background = element_blank()) +
  scale_y_continuous(limits = c(0, 10), breaks = c(2, 4, 6, 8)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = .5))
plot.pointsbypop

#Study Data Points for Age = Adult only
data.adults <- subset(data.agecontext, Participant.Age.Bin == "Adult")
plot.pointsbypop.adult <- ggplot(data.adults, aes(Data.Points)) + 
  geom_bar(stat = "count", color = "black", fill = "white", position = "dodge") +
  theme_bw() +
  xlab("\nStudy Data Points") +
  ylab("") +
  facet_wrap(~Context, ncol = 1, strip.position = "right") +
  theme(strip.background = element_blank()) +
  scale_y_continuous(limits = c(0, 10), breaks = c(2, 4, 6, 8)) +
  ggtitle("\nAdult") +
  theme(plot.title = element_text(size = 9, hjust = .5, vjust = 1)) +
  theme(axis.title.y = NULL)
plot.pointsbypop.adult

#Arrange and save these plots
library(ggpubr)
participantcharacteristics <- ggarrange(plot.lengthbypop, plot.pointsbypop.adult, widths = c(1,.5))
participantcharacteristics
ggsave("participant characteristics plot - younger v adult.jpg", plot = participantcharacteristics, width = 6.5, height = 4.5, dpi = 300)

#Longitudinal by Measurement Framework (Saito & Plonsky, 2019) ----
#Remove perception studies
data.sp.production <- subset(data.SP, SP.Task == "Production - Controlled" | SP.Task == "Production - Spontaneous")
data.sp.production$SP.Task <- factor(data.sp.production$SP.Task, levels = c("Production - Controlled", "Production - Spontaneous"), labels = c("Controlled", "Spontaneous"))
data.sp.production$SP.Construct <- factor(data.sp.production$SP.Construct, levels = c("Production - Global", "Production - Specific"), labels = c("Global", "Specific"))

#Study Length
plot.lengthbysp <- ggplot(data.sp.production, aes(Length.Bin)) + 
  geom_bar(stat = "count", color = "black", fill = "white", position = "dodge") +
  theme_bw() +
  xlab("Study Length") +
  ylab("Count") +
  facet_grid(SP.Construct ~ SP.Task) +
  theme(strip.background = element_blank()) +
  scale_y_continuous(limits = c(0, 10), breaks = c(2, 4, 6, 8)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = .5))
plot.lengthbysp

#Study Data Points
data.sp.production$Data.Points <- as.factor(data.sp.production$Data.Points)
plot.pointsbysp <- ggplot(data.sp.production, aes(Data.Points)) + 
  geom_bar(stat = "count", color = "black", fill = "white", position = "dodge") +
  theme_bw() +
  xlab("Study Data Points") +
  ylab("Count") +
  facet_grid(SP.Construct ~ SP.Task) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  theme(strip.background = element_blank()) +
  scale_y_continuous(limits = c(0, 10), breaks = c(2, 4, 6, 8))
plot.pointsbysp

#Arrange and save
plot.sp <- ggarrange(plot.lengthbysp, plot.pointsbysp, ncol = 2)
ggsave("sp characteristics plot.jpg", plot = plot.sp, width = 6.5, height = 4, dpi = 300)
