#brief tutorial on how to plot mouse trajectory data from a 2 x 2 repeated-measures experiment in MouseTracker

#only packages you'll need (for reshaping and for creating the graphs)
library(tidyverse); library(gridExtra)

#first step is to load the data
dataFrame <- read.csv("data/mouseTrackingStudyData.csv", header = T) #not the name of any actual data, just a filler name; input your own data file's name!

#specify any conditions by words, if desired
dfCondition$condition <- ifelse(dataFrame$condition == 2, "Condition2", "Condition1")

#select the appropriate columns; these include participant numbers, conditions, and the x and y coordinates
dfX <- dataFrame[,c(1:3,12:112)] 
dfY <- dataFrame[,c(1:3,113:213)]


#now for the plotting
#start by gathering them into long form

#do the x-coordinates first
dfLongX <- gather(dfX, key = "xSpace", value = "xCoord",
                  X_1,X_2,X_3,X_4,X_5,X_6,X_7,X_8,X_9,X_10,
                  X_11,X_12,X_13,X_14,X_15,X_16,X_17,X_18,X_19,X_20,
                  X_21,X_22,X_23,X_24,X_25,X_26,X_27,X_28,X_29,X_30,
                  X_31,X_32,X_33,X_34,X_35,X_36,X_37,X_38,X_39,X_40,
                  X_41,X_42,X_43,X_44,X_45,X_46,X_47,X_48,X_49,X_50,
                  X_51,X_52,X_53,X_54,X_55,X_56,X_57,X_58,X_59,X_60,
                  X_61,X_62,X_63,X_64,X_65,X_66,X_67,X_68,X_69,X_70,
                  X_71,X_72,X_73,X_74,X_75,X_76,X_77,X_78,X_79,X_80,
                  X_81,X_82,X_83,X_84,X_85,X_86,X_87,X_88,X_89,X_90,
                  X_91,X_92,X_93,X_94,X_95,X_96,X_97,X_98,X_99,X_100,
                  X_101)

#now the y-coordinates
dfLongY <- gather(dfY, key = "ySpace", value = "yCoord",
                  Y_1,Y_2,Y_3,Y_4,Y_5,Y_6,Y_7,Y_8,Y_9,Y_10,
                  Y_11,Y_12,Y_13,Y_14,Y_15,Y_16,Y_17,Y_18,Y_19,Y_20,
                  Y_21,Y_22,Y_23,Y_24,Y_25,Y_26,Y_27,Y_28,Y_29,Y_30,
                  Y_31,Y_32,Y_33,Y_34,Y_35,Y_36,Y_37,Y_38,Y_39,Y_40,
                  Y_41,Y_42,Y_43,Y_44,Y_45,Y_46,Y_47,Y_48,Y_49,Y_50,
                  Y_51,Y_52,Y_53,Y_54,Y_55,Y_56,Y_57,Y_58,Y_59,Y_60,
                  Y_61,Y_62,Y_63,Y_64,Y_65,Y_66,Y_67,Y_68,Y_69,Y_70,
                  Y_71,Y_72,Y_73,Y_74,Y_75,Y_76,Y_77,Y_78,Y_79,Y_80,
                  Y_81,Y_82,Y_83,Y_84,Y_85,Y_86,Y_87,Y_88,Y_89,Y_90,
                  Y_91,Y_92,Y_93,Y_94,Y_95,Y_96,Y_97,Y_98,Y_99,Y_100,
                  Y_101)

#now combine them
dfPointsLong <- cbind(dfLongX[,c(1:5)], dfLongY[,c(4,5)])

#split them to create two different plots (categorizations of conditions 1 and 2)
dfPLCon1 <- filter(dfPointsLong, condition == "Condition 1")
dfPLCon2 <- filter(dfPointsLong, condition == "Condition 2")

dfLSummary1 <- dfPLCon1 %>%
  group_by(condition, xSpace, ySpace) %>%
  summarize(
    meanX = mean(xCoord),
    meanY = mean(yCoord)
  ) %>%
  as.data.frame

dfLSummary2 <- dfPLCon2 %>%
  group_by(condition, xSpace, ySpace) %>%
  summarize(
    meanX = mean(xCoord),
    meanY = mean(yCoord)
  ) %>%
  as.data.frame

(cond1 <- ggplot(dfLSummary1, aes(meanX, meanY, color = condition)) +
    geom_point(aes(group = condition, shape = condition)) + 
    geom_line(aes(group = condition, linetype = condition)) +
    geom_segment(x = 0, xend = -.89, y = .05, yend = 1.44, color = "black") +
    annotate(geom = "text", x = -.9, y = 1.47, label = "ANYTHING", color = "black") +
    annotate(geom = "text", x = .9, y = 1.47, label = "ANYTHING", color = "black") +
    scale_y_continuous(expand = c(0, 0)) + expand_limits(x = c(-1.00, 1.00), y = c(0, 1.52)) +
    scale_color_grey() + xlab("x-coordinate") + ylab("y-coordinate") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    guides(color = guide_legend(title = "Condition"),
           group = guide_legend(title = "Condition"),
           shape = guide_legend(title = "Condition"),
           linetype = guide_legend(title = "Condition")))

(cond2 <- ggplot(dfLSummary2, aes(meanX, meanY, color = condition)) +
    geom_point(aes(group = condition, shape = condition)) + 
    geom_line(aes(group = condition, linetype = condition)) +
    geom_segment(x = 0, xend = .9, y = .05, yend = 1.44, color = "black") +
    annotate(geom = "text", x = -.9, y = 1.47, label = "ANYTHING", color = "black") +
    annotate(geom = "text", x = .9, y = 1.47, label = "ANYTHING", color = "black") +
    scale_y_continuous(expand = c(0, 0)) + expand_limits(x = c(-1.00, 1.00), y = c(0, 1.52)) +
    scale_color_grey() + xlab("x-coordinate") + ylab("y-coordinate") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    guides(color = guide_legend(title = "Condition"),
           group = guide_legend(title = "Condition"),
           shape = guide_legend(title = "Condition"),
           linetype = guide_legend(title = "Condition")))


#now combine the plots into single figures
ggarrange(cond1, cond2, labels = c("A", "B"), nrow = 2, ncol = 1)
