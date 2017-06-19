# Author: Kyle B. Schenthal
# Last Updated: Jan 25 2017
# Udacity Course: Statistics, The Science of Decision Making
# Project: Stroop Effect Analysis
# Description:  Analysis of the stroop effect experimental data from Udacity
#               course for project report. Includes table of descriptive
#               statistics (Q3), data visualizations (Q4), and t-test (Q5)
# ---------------------------------------------------------
# Load Data
stroop_data <- read.csv('stroop.csv')
stroop_data$Difference <- stroop_data$Incongruent - stroop_data$Congruent


# Descriptive Statistics Table-----------------------------
descriptive <- do.call(data.frame,
                       list(Mean = apply(stroop_data, 2, mean),
                            Std_Dev = apply(stroop_data, 2, sd),
                            Min = apply(stroop_data, 2, min),
                            Q1 = apply(stroop_data, 2, quantile, probs = 0.25),
                            Median = apply(stroop_data, 2, median),
                            Q1 = apply(stroop_data, 2, quantile, probs = 0.75),
                            Max = apply(stroop_data, 2, max),
                            IQR = apply(stroop_data, 2, IQR),
                            Range = apply(stroop_data, 2, max) -
                            apply(stroop_data, 2, min) ,
                            Size = apply(stroop_data, 2, length),
                            SEM = apply(stroop_data, 2, sd) /
                                sqrt(apply(stroop_data,2, length))
                       ))
print(t(format(round(descriptive, 2), nsmall = 2)))


# Plot Data------------------------------------------------
# Boxplot
boxplot(stroop_data$Congruent, stroop_data$Incongruent,
        names = c('Congruent', 'Incongruent'),
        col = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)),
        main = 'Stroop Performance by Condition',
        xlab = 'Condition', ylab = 'Response Time (s)')

# Congruent Histogram
congruent_hist <- hist(stroop_data$Congruent, breaks = seq(7, 23, 2),
                       col = rgb(1,0,0,0.5), xlab = 'Response Time (s)', main =
                           'Histogram of Congruent Condition Response Times')
congruent_multiplier <- congruent_hist$counts / congruent_hist$density
congruent_density <- density(stroop_data$Congruent)
congruent_density$y <- congruent_density$y * congruent_multiplier[1]
lines(congruent_density)

# Incongruent Histogram
incongruent_hist <- hist(stroop_data$Incongruent, breaks = seq(14, 36, 2),
                         col = rgb(0,0,1,0.5), xlab = 'Response Time (s)', main =
                             'Histogram of Incongruent Condition Response Times')
incongruent_multiplier <- incongruent_hist$counts / incongruent_hist$density
incongruent_density <- density(stroop_data$Incongruent)
incongruent_density$y <- incongruent_density$y * incongruent_multiplier[1]
lines(incongruent_density)

# Overlapping Histogram
hist(stroop_data$Congruent, breaks = seq(7, 37, 2), col = rgb(1,0,0,0.5),
     xlab = 'Response Time (s)', main = 'Overlapping Histograms of Congruent Response
     Times by Condition')
hist(stroop_data$Incongruent, breaks = seq(14, 36, 2), col = rgb(0,0,1,0.5), add = T)
box()
lines(incongruent_density, col = 'blue')
lines(congruent_density, col = 'red')
legend(x = 'topright', legend = c('Congruent', 'Incongruent'),
       fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))


# Statistical Test-----------------------------------------
stroop_t <- t.test(stroop_data$Incongruent, stroop_data$Congruent, 
       alt = 'greater', paired = T)
print(stroop_t)