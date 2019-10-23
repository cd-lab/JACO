#for .pdf figures, replace all instances of ".png" with ".pdf"

#BASE OPTIONS
options(expressions=10000)

#LOAD LIBRARIES (nonnest2, merDeriv loaded at end; they interact poorly with summary())
library(lsr)
library(pwr)
library(beeswarm)
library(lme4)
library(plotrix)
library(rstan)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
library(brms)


#WORKING DIRECTORIES
wds = c('../Data/Sample 1',
        '../Sample 2',
        '../Combined Sample')
tags = c('1',
         '2',
         'Combined')

#SET GRAPHICAL PARAMETERS
cex = 1.5
font.main = 1
cex.main = 2.25
cex.axis = 3.5
cex.lab = 2
cex.leg = 1.125
lwd = 3
lwdThin = 2
toma = 1
roma = 1
loma = 1
boma = 1

#LOOP THROUGH ALL WORKING DIRECTORIES AND RUN SAME ANALYSES
for (wd in 1:length(wds)) {

  #this working directory
  setwd(wds[wd])
  
  #load data
  allFiles = list.files(pattern='Task.csv')
  allParticipantData = list()
  for (i in 1:length(allFiles)) {
    allParticipantData[[i]] = read.csv(allFiles[i])
  }
  
  #list of participant numbers
  allParticipantNumbers = numeric(length(allFiles))
  for (i in 1:length(allFiles)) {
    allParticipantNumbers[i] = substring(allFiles[i], 12, 14)
  }
  
  #add participant number column
  for (i in 1:length(allFiles)) {
    allParticipantData[[i]]$participantNumber = allParticipantNumbers[i]
  }
  
  #create giant dataframe of all subjects
  allData = allParticipantData[[1]]
  for (i in 2:length(allParticipantData)) {
    allData = rbind(allData, allParticipantData[[i]])
  }
  
  #make unanswered trials get half a point
  allData[allData$reactionTime >= 4.0,]$participantCorrect = NA
  
  #create data frames for each block
  block1 = allData[allData$block == 1,]
  block2 = allData[allData$block == 2,]
  block3 = allData[allData$block == 3,]
  block4 = allData[allData$block == 4,]
  allBlocks = list()
  allBlocks[[1]] = block1
  allBlocks[[2]] = block2
  allBlocks[[3]] = block3
  allBlocks[[4]] = block4
  performanceBlocks = block2
  performanceBlocks = rbind(performanceBlocks, block3)
  performanceBlocks = rbind(performanceBlocks, block4)
  
  #create empty vectors to populate with performance variables
  vectorLength = (4 * length(allFiles)) + 4
  participantNumber = character(vectorLength)
  blockNumber = numeric(vectorLength)
  notAnswered = numeric(vectorLength)
  probeCount = numeric(vectorLength)
  responseRateAnswered = numeric(vectorLength)
  responseRate = numeric(vectorLength)
  pValue = numeric(vectorLength)
  CILow = numeric(vectorLength)
  CIHigh = numeric(vectorLength)
  
  #BIG LOOP TO CREATE SUMMARY DATA FRAME
  index = 1
  for (i in 1:length(allBlocks)) {
    block = allBlocks[[i]]
    blockProbes = block[block$probe == 1,] #all trials
    blockAnsweredProbes = blockProbes[blockProbes$participantResponse == 'right'| blockProbes$participantResponse == 'left',] #answered trials
    #analyze aggregate statistics with binomial test (wilcoxon used later)
    binomTest = binom.test(sum(blockAnsweredProbes$participantCorrect),
                           nrow(blockAnsweredProbes),
                           p=0.5,
                           alternative='two.sided',
                           conf.level=0.95)
    participantNumber[index] = 'Average'
    blockNumber[index] = i
    notAnswered[index] = nrow(blockProbes) - nrow(blockAnsweredProbes)
    probeCount[index] = nrow(blockProbes)
    responseRateAnswered[index] = sum(blockAnsweredProbes$participantCorrect)/nrow(blockAnsweredProbes)
    responseRate[index] = sum(blockProbes[!is.na(blockProbes$participantCorrect),]$participantCorrect)/nrow(blockProbes)
    pValue[index] = binomTest[[3]]
    CILow[index] = binomTest[[4]][1]
    CIHigh[index] = binomTest[[4]][2]
    index = index + 1
    #do the above analyses for every individual
    for (j in 1:length(allFiles)) {
      blockProbesParticipant = blockProbes[blockProbes$participantNumber == allParticipantNumbers[j],]
      blockAnsweredProbesParticipant = blockProbesParticipant[blockProbesParticipant$participantResponse == 'right'| blockProbesParticipant$participantResponse == 'left',]
      binomTest = list(NA, NA, NA, list(NA, NA))
      if (nrow(blockAnsweredProbesParticipant) > 0) {
        binomTest = binom.test(sum(blockAnsweredProbesParticipant$participantCorrect),
                               nrow(blockAnsweredProbesParticipant),
                               p=0.5,
                               alternative='two.sided',
                               conf.level=0.95)
      }
      participantNumber[index] = allParticipantNumbers[j]
      blockNumber[index] = i
      notAnswered[index] = nrow(blockProbesParticipant) - nrow(blockAnsweredProbesParticipant)
      probeCount[index] = nrow(blockProbesParticipant)
      responseRateAnswered[index] = sum(blockAnsweredProbesParticipant$participantCorrect)/nrow(blockAnsweredProbesParticipant)
      responseRate[index] = sum(blockProbesParticipant[!is.na(blockProbesParticipant$participantCorrect),]$participantCorrect)/nrow(blockProbesParticipant)
      pValue[index] = binomTest[[3]]
      CILow[index] = binomTest[[4]][[1]]
      CIHigh[index] = binomTest[[4]][[2]]
      index = index + 1
    }
  }
  
  #make summary data frame
  summaryData = data.frame(participantNumber,
                           blockNumber,
                           notAnswered,
                           probeCount,
                           responseRateAnswered,
                           responseRate,
                           pValue,
                           CILow,
                           CIHigh)
  if (wd %in% c(1, 2)) {
    print(paste("Sample", wd, "group accuracy in Block 2:", summaryData[summaryData$blockNumber == 2 & summaryData$participantNumber == 'Average',]$responseRateAnswered))
    print(paste("Sample", wd, "group standard deviation in Block 2:", sd(summaryData[summaryData$blockNumber == 2 & summaryData$participantNumber != 'Average',]$responseRateAnswered)))
    print(paste("Sample", wd, "group accuracy in Block 3:", summaryData[summaryData$blockNumber == 3 & summaryData$participantNumber == 'Average',]$responseRateAnswered))
    print(paste("Sample", wd, "group standard deviation in Block 3:", sd(summaryData[summaryData$blockNumber == 3 & summaryData$participantNumber != 'Average',]$responseRateAnswered)))
    print(paste("Sample", wd, "group accuracy in Block 4:", summaryData[summaryData$blockNumber == 4 & summaryData$participantNumber == 'Average',]$responseRateAnswered))
    print(paste("Sample", wd, "group standard deviation in Block 3:", sd(summaryData[summaryData$blockNumber == 4 & summaryData$participantNumber != 'Average',]$responseRateAnswered)))
  } else {
    print(paste("Combined Sample group accuracy in Block 2:", summaryData[summaryData$blockNumber == 2 & summaryData$participantNumber == 'Average',]$responseRateAnswered))
    print(paste("Combined Sample group standard deviation in Block 2:", sd(summaryData[summaryData$blockNumber == 2 & summaryData$participantNumber != 'Average',]$responseRateAnswered)))
    print(paste("Combined Sample group accuracy in Block 3:", summaryData[summaryData$blockNumber == 3 & summaryData$participantNumber == 'Average',]$responseRateAnswered))
    print(paste("Combined Sample group standard deviation in Block 3:", sd(summaryData[summaryData$blockNumber == 3 & summaryData$participantNumber != 'Average',]$responseRateAnswered)))
    print(paste("Combined Sample group accuracy in Block 4:", summaryData[summaryData$blockNumber == 4 & summaryData$participantNumber == 'Average',]$responseRateAnswered))
    print(paste("Combined Sample group standard deviation in Block 4:", sd(summaryData[summaryData$blockNumber == 4 & summaryData$participantNumber != 'Average',]$responseRateAnswered)))
  }
  
  #wilcoxon signed-rank test for group performance in block 1
  wilcoxTest = wilcox.test(summaryData[summaryData$blockNumber == 1 & summaryData$participantNumber != 'Average',]$responseRateAnswered,
                           alternative='two.sided',
                           mu=0.5,
                           exact=FALSE)
  if (wd %in% c(1, 2)) {
    print(paste("Sample", wd, "Block 1 group accuracy p-value:", wilcoxTest[[3]]))
  } else {
    print(paste("Combined Sample Block 1 group accuracy p-value:", wilcoxTest[[3]]))
  }
  
  #wilcoxon signed-rank test for group performance in block 2
  wilcoxTest = wilcox.test(summaryData[summaryData$blockNumber == 2 & summaryData$participantNumber != 'Average',]$responseRateAnswered,
                           alternative='two.sided',
                           mu=0.5,
                           exact=FALSE)
  if (wd %in% c(1, 2)) {
    print(paste("Sample", wd, "Block 2 group accuracy p-value:", wilcoxTest[[3]]))
  } else {
    print(paste("Combined Sample Block 2 group accuracy p-value:", wilcoxTest[[3]]))
  }
  
  #wilcoxon signed-rank test for group performance in block 3
  wilcoxTest = wilcox.test(summaryData[summaryData$blockNumber == 3 & summaryData$participantNumber != 'Average',]$responseRateAnswered,
                           alternative='two.sided',
                           mu=0.5,
                           exact=FALSE)
  if (wd %in% c(1, 2)) {
    print(paste("Sample", wd, "Block 3 group accuracy p-value:", wilcoxTest[[3]]))
  } else {
    print(paste("Combined Sample Block 3 group accuracy p-value:", wilcoxTest[[3]]))
  }
  
  #wilcoxon signed-rank test for group performance in block 4
  wilcoxTest = wilcox.test(summaryData[summaryData$blockNumber == 4 & summaryData$participantNumber != 'Average',]$responseRateAnswered,
                           alternative='two.sided',
                           mu=0.5,
                           exact=FALSE)
  if (wd %in% c(1, 2)) {
    print(paste("Sample", wd, "Block 4 group accuracy p-value:", wilcoxTest[[3]]))
  } else {
    print(paste("Combined Sample Block 4 group accuracy p-value:", wilcoxTest[[3]]))
  }
  
  #get high-performing (good) and not-significantly-performing (low) participants
  goodParticipantNumbers = c()
  goodParticipantPerformances = c()
  badParticipantNumbers = c()
  badParticipantPerformances = c()
  for (i in 1:length(allParticipantNumbers)) {
    thisNumber = allParticipantNumbers[i]
    thisPerformance = summaryData[summaryData$participantNumber == thisNumber & summaryData$blockNumber == 4,]$pValue
    if (thisPerformance <= 0.05) {
      goodParticipantNumbers = c(goodParticipantNumbers, thisNumber)
      goodParticipantPerformances = c(goodParticipantPerformances, summaryData[summaryData$participantNumber == thisNumber & summaryData$blockNumber == 4,]$responseRateAnswered)
    } else {
      badParticipantNumbers = c(badParticipantNumbers, thisNumber)
      badParticipantPerformances = c(badParticipantPerformances, summaryData[summaryData$participantNumber == thisNumber & summaryData$blockNumber == 4,]$responseRateAnswered)
    }
  }
  if (wd %in% c(1, 2)) {
    print(paste("Sample", wd, "number of high performers in Block 4:", length(goodParticipantNumbers)))
  } else {
    print(paste("Combined Sample number of high performers in Block 4:", length(goodParticipantNumbers)))
  }
  
  #calculate effect size and replication sample size based on this sample
  d = cohensD(c(goodParticipantPerformances, badParticipantPerformances), mu=0.5)
  n = pwr.t.test(sig.level=0.05, power=.8, d = d, alt="greater", n = NULL, type='one.sample')[[1]]
  if (wd %in% c(1, 2)) {
    print(paste("Sample", wd, "effect size in Block 4:", d))
    print(paste("Sample", wd, "replication size from Block 4 effect size:", n))
  } else {
    print(paste("Combined Sample effect size in Block 4:", d))
    print(paste("Combined Sample replication size from Block 4 effect size", n))
  }
  
  #only do further analyses for combined sample
  if (tags[wd] == 'Combined') {
  
    #change directory for outputs
    setwd('../../Figures')
    
    #PLOT ACCURACY BY BLOCK NUMBER
    png('accuracyByTestingBlock.png',width=6,height=6,units='in',res=500)
    averageSummary = summaryData[summaryData$participantNumber == 'Average',]
    par(xpd=TRUE,
        mai=c(boma, loma, toma, roma))
    plot(averageSummary$blockNumber,
         averageSummary$responseRateAnswered,
         xlim=c(2,4),
         ylim=c(0,1),
         main='Accuracy by Testing Block',
         xlab='Block Number',
         ylab='Accuracy',
         pch=20,
         cex=cex,
         type='n',
         font.main=font.main,
         cex.main=cex.main,
         cex.axis=cex.axis,
         cex.lab=cex.lab,
         lty=1,
         lwd=lwd,
         axes=FALSE,
         col='black')
    axis(side=1, at=c(2:4))
    axis(side=2, at=seq(0,1,0.5))
    for (i in 1:length(allFiles)) {
      participantPerformanceSummary = summaryData[summaryData$participantNumber == allParticipantNumbers[i] & summaryData$blockNumber != 1,]
      points(participantPerformanceSummary$blockNumber,
             participantPerformanceSummary$responseRateAnswered,
             pch=20,
             cex=cex,
             col='grey')
      lines(participantPerformanceSummary$blockNumber,
            participantPerformanceSummary$responseRateAnswered,
            lty=1,
            lwd=lwdThin,
            col='grey')
    }
    points(averageSummary[averageSummary$blockNumber != 1,]$blockNumber,
           averageSummary[averageSummary$blockNumber != 1,]$responseRateAnswered,
           pch=20,
           col='black',
           cex=cex)
    lines(averageSummary[averageSummary$blockNumber != 1,]$blockNumber,
          averageSummary[averageSummary$blockNumber != 1,]$responseRateAnswered,
          lty=1,
          lwd=lwd)
    lines(c(1.95,4.05),
          c(0.5, 0.5),
          lty='dashed',
          col='black',
          lwd=lwdThin)
    legend('bottomright',
           legend=c('Group trend', 'Individual trend'),
           pch=c(20,20),
           lty=c(1,1),
           col=c('black', 'grey'),
           cex=cex.leg)
    dev.off()
    
    #declare trial indices
    probeNumber = seq(1, 30)
    block1Probes = block1[block1$probe == 1,]
    block1Probes$probeNumber = seq(1,15)
    block2Probes = block2[block2$probe == 1,]
    block2Probes$probeNumber = probeNumber
    block3Probes = block3[block3$probe == 1,]
    block3Probes$probeNumber = probeNumber
    block4Probes = block4[block4$probe == 1,]
    block4Probes$probeNumber = probeNumber
    performanceBlocksProbes = performanceBlocks[performanceBlocks$probe == 1,]
    
    #PLOT TRAINING BLOCK ACCURACY
    png('accuracyTraining.png',width=6,height=6,units='in',res=500)
    par(mai=c(boma, loma, toma, roma))
    beeswarm(summaryData[summaryData$blockNumber == 1,]$responseRateAnswered,
             xlim=c(0.9, 1.1),
             ylim=c(0,1),
             main='Accuracy in Training Phase',
             ylab='Accuracy',
             pch=20,
             cex=cex,
             font.main=font.main,
             cex.main=cex.main,
             cex.axis=cex.axis,
             cex.lab=cex.lab,
             lty=1,
             lwd=lwd,
             axes=FALSE,
             col='grey',
             method='center')
    axis(side=2, at=c(0, 0.5, 1))
    print(paste("Combined Sample median participant accuracy in Block 1:", median(summaryData[summaryData$blockNumber == 1 & summaryData$participantNumber != 'Average' & !is.na(summaryData$responseRateAnswered),]$responseRateAnswered)))
    print(paste("Combined Sample standard deviation of participant accuracy in Block 1:", sd(summaryData[summaryData$blockNumber == 1 & summaryData$participantNumber != 'Average' & !is.na(summaryData$responseRateAnswered),]$responseRateAnswered)))
    legend('bottomright',
           legend=c('Individual'),
           pch=c(20),
           col=c('grey'),
           cex=cex.leg)
    dev.off()
    
    #prepare to plot learning across trials in training block
    trainingPerformance = numeric(15)
    for (i in 1:15) {
      trainingPerformance[i] = mean(block1Probes[block1Probes$probeNumber == i & !is.na(block1Probes$participantCorrect),]$participantCorrect)
    }
    
    #PLOT LEARNING ACROSS TRIALS IN TRAINING BLOCK
    png('learningTraining.png',width=6,height=6,units='in',res=500)
    par(mai=c(boma, loma, toma, roma))
    plot(1:15,
         trainingPerformance,
         xlab='Trial Index',
         ylab='Accuracy',
         ylim=c(0,1),
         main='Learning Across Training Trials',
         col='grey',
         xlim=c(0,15),
         pch=20,
         cex=cex,
         font.main=font.main,
         lwd=lwd,
         cex.main=cex.main,
         cex.axis=cex.axis,
         cex.lab=cex.lab,
         axes=FALSE)
    axis(side = 1,
         at = c(0, 5, 10, 15),
         labels=c('', '5', '10', '15'))
    axis(side = 2,
         at = c(0.0, 0.5, 1.0))
    x = seq(0, 15, 0.001)
    model = glmer(participantCorrect ~ probeNumber + (probeNumber||participantNumber),
                  family=binomial(link='probit'),
                  data=block1Probes[!is.na(block1Probes$participantCorrect),]) #mixed-effects probit regression
    intercept = fixef(model)[[1]]
    effect = fixef(model)[[2]]
    lines(x,
          pnorm(intercept + (effect*x)),
          lwd=lwd)
    print(paste("Combined Sample final training block accuracy:", pnorm(intercept + (effect*max(x)))))
    legend('bottomright',
           legend=c('Per trial', 'Group trend'),
           pch=c(20, NA),
           lty=c(NA, 1),
           col=c('grey', 'black'),
           cex=cex.leg)
    dev.off()
    
    #get training response times
    medianRTsTraining = c()
    for (i in 1:length(allParticipantNumbers)) {
      medianRTsTraining = c(medianRTsTraining, median(block1Probes[block1Probes$participantNumber == allParticipantNumbers[i] & block1Probes$participantCorrect %in% c(1),]$reactionTime))
    }
    medianRTsTraining = c(medianRTsTraining[1:16], medianRTsTraining[18:25])
    print(paste("Combined Sample mean participant response time in Block 1:", mean(medianRTsTraining)))
    print(paste("Combined Sample standard deviation of participant response times in Block 1:", sd(medianRTsTraining)))
    
    #prepare for plotting learning across testing blocks
    allTrialIndices = c(block2Probes$probeNumber, block3Probes$probeNumber + 30, block4Probes$probeNumber + 60)
    trialIndices = 1:90
    allTrialPerformances = c(block2Probes$participantCorrect, block3Probes$participantCorrect, block4Probes$participantCorrect)
    allTrialParticipantNumbers = c(block2Probes$participantNumber, block3Probes$participantNumber, block4Probes$participantNumber)
    averageTrialPerformances = numeric(90)
    for (i in trialIndices) {
      if (i < 31) {
        averageTrialPerformances[i] = mean(block2Probes[block2Probes$probeNumber == i & !is.na(block2Probes$participantCorrect),]$participantCorrect)
      } else if (i < 61) {
        averageTrialPerformances[i] = mean(block3Probes[block3Probes$probeNumber == i-30 & !is.na(block3Probes$participantCorrect),]$participantCorrect)
      } else {
        averageTrialPerformances[i] = mean(block4Probes[block4Probes$probeNumber == i-60 & !is.na(block4Probes$participantCorrect),]$participantCorrect)
      }
    }
    allTrialDF = data.frame(allTrialIndices/90,
                            allTrialPerformances,
                            allTrialParticipantNumbers)
    colnames(allTrialDF) = c('index',
                             'correct',
                             'number')
    
    #PLOT LEARNING ACROSS TESTING BLOCKS
    png('learningTesting.png',width=6,height=6,units='in',res=500)
    par(mai=c(boma, loma, toma, roma))
    plot(trialIndices,
         averageTrialPerformances,
         xlab='Trial Index',
         ylab='Accuracy',
         ylim=c(0,1),
         main='Learning Across Testing Trials',
         col='darkgrey',
         xlim=c(0,90),
         pch=20,
         cex=cex,
         font.main=font.main,
         lwd=lwd,
         cex.main=cex.main,
         cex.axis=cex.axis,
         cex.lab=cex.lab,
         axes=FALSE)
    axis(side = 1,
         at = c(0, 30, 60, 90),
         labels = c('', 30, 60, 90))
    axis(side = 2,
         at = c(0.0, 0.5, 1.0))
    x = seq(0, 90, 0.001)
    for (i in 1:length(allParticipantNumbers)) {
      testParticipantDF = data.frame(trialIndices,
                                     performanceBlocksProbes[performanceBlocksProbes$participantNumber == allParticipantNumbers[[i]],]$participantCorrect)
      colnames(testParticipantDF) = c('index', 'participantCorrect')
      model = glm(participantCorrect ~ index,
                  family=binomial(link='probit'),
                  data=testParticipantDF[!is.na(testParticipantDF),]) #probit regression
      intercept = model[[1]][[1]]
      effect = model[[1]][[2]]
      lines(x,
            pnorm(intercept + (effect*x)),
            col='lightgrey',
            lwd=lwdThin)
    }
    model = glmer(correct ~ index + (index||number),
                  family=binomial(link='probit'),
                  data=allTrialDF[!is.na(allTrialDF$correct),]) #mixed-effects probit regression
    intercept = fixef(model)[[1]]
    effect = fixef(model)[[2]]
    print(paste("Combined Sample effect of testing trial number on accuracy p-value:", summary(model)[[10]][[8]]))
    print(paste("Combined Sample estimated mean accuracy at start of testing phase:", pnorm(intercept + (effect*(1/90)))))
    print(paste("Combined Sample estimated mean accuracy at end of testing phase:", pnorm(intercept + (effect*1))))
    x = seq(0, 1, 0.00001)
    lines(x*90,
          pnorm(intercept + (effect*x)),
          lwd=lwd)
    mtext('Block 2',
          1,
          line=0.5,
          at=15)
    mtext('Block 3',
          1,
          line=0.5,
          at=45)
    mtext('Block 4',
          1,
          line=0.5,
          at=75)
    legend('bottomright',
           legend=c('Per trial', 'Group trend', 'Individual trend'),
           lty=c(NA,1,1),
           pch=c(20,NA,NA),
           col=c('darkgrey', 'black', 'lightgrey'),
           cex=cex.leg)
    dev.off()
    
    #PLOT SIGNAL ACROSS TESTING BLOCKS
    png('signalTesting.png',width=6,height=6,units='in',res=500)
    par(mai=c(boma, loma, toma, roma))
    plot(trialIndices,
         averageTrialPerformances,
         xlab='Trial Index',
         ylab='Sensitivity Index (d\')',
         ylim=c(-1,4),
         main='Signal Across Testing Trials',
         col='darkgrey',
         xlim=c(0,90),
         pch=20,
         cex=cex,
         font.main=font.main,
         lwd=lwd,
         cex.main=cex.main,
         cex.axis=cex.axis,
         cex.lab=cex.lab,
         axes=FALSE,
         type='n')
    axis(side = 1,
         at = c(0, 30, 60, 90),
         labels = c('', 30, 60, 90))
    axis(side = 2,
         at = c(-1.0, 0.0, 1.0, 2.0, 3.0, 4.0),
         labels = c('', '0', '', '2', '', '4'))
    x = seq(0, 90, 0.001)
    for (i in 1:length(allParticipantNumbers)) {
      testParticipantDF = data.frame(trialIndices,
                                     performanceBlocksProbes[performanceBlocksProbes$participantNumber == allParticipantNumbers[[i]],]$participantCorrect)
      colnames(testParticipantDF) = c('index', 'participantCorrect')
      model = glm(participantCorrect ~ index,
                  family=binomial(link='probit'),
                  data=testParticipantDF[!is.na(testParticipantDF),]) #probit regression
      intercept = model[[1]][[1]]
      effect = model[[1]][[2]]
      lines(x,
            sqrt(2)*(intercept + (effect*x)),
            col='grey',
            lwd=lwdThin)
    }
    model = glmer(correct ~ index + (index||number),
                  family=binomial(link='probit'),
                  data=allTrialDF[!is.na(allTrialDF$correct),]) #mixed-effects probit regression
    intercept = fixef(model)[[1]]
    effect = fixef(model)[[2]]
    print(paste("Combined Sample estimated sensitivity index at start of testing phase:", sqrt(2)*(intercept + (effect*(1/90)))))
    print(paste("Combined Sample estimated sensitivity index at end of testing phase:", sqrt(2)*(intercept + (effect*1))))
    x = seq(0, 1, 0.00001)
    lines(x*90,
          sqrt(2)*(intercept + (effect*x)),
          lwd=lwd)
    mtext('Block 2',
          1,
          line=0.5,
          at=15)
    mtext('Block 3',
          1,
          line=0.5,
          at=45)
    mtext('Block 4',
          1,
          line=0.5,
          at=75)
    legend('bottomright',
           legend=c('Group trend', 'Individual trend'),
           lty=c(1,1,NA),
           col=c('black', 'grey'),
           cex=cex.leg)
    dev.off()
    
    #correlation between training and testing performances
    corTest = cor.test(summaryData[summaryData$blockNumber == 1 & !(summaryData$participantNumber %in% c('Average', 500)),]$responseRateAnswered,
                       summaryData[summaryData$blockNumber == 4 & !(summaryData$participantNumber %in% c('Average', 500)),]$responseRateAnswered,
                       method='spearman',
                       exact=FALSE)
    print(paste('Combined Sample correlation value between Block 1 and Block 4 accuracy:', corTest[[4]][[1]]))
    print(paste('Combined Sample correlation between Block 1 and Block 4 accuracy p-value:', corTest[[3]]))
    
    #sensitivity index - beginning and end relative to chance
    beginningDPrime = c()
    finalDPrime = c()
    for (i in 1:25) {
      testParticipantDF = data.frame(trialIndices,
                                     performanceBlocksProbes[performanceBlocksProbes$participantNumber == allParticipantNumbers[[i]],]$participantCorrect)
      colnames(testParticipantDF) = c('index', 'participantCorrect')
      model = glm(participantCorrect ~ index,
                  family=binomial(link='probit'),
                  data=testParticipantDF[!is.na(testParticipantDF),]) #probit regression
      intercept = model[[1]][[1]]
      effect = model[[1]][[2]]
      beginningDPrime = c(beginningDPrime, sqrt(2)*(intercept + effect))
      finalDPrime = c(finalDPrime, sqrt(2)*(intercept + (effect*90)))
    }
    wilcox.test(beginningDPrime,
                alternative='two.sided',
                mu=0,
                exact=TRUE)
    wilcox.test(finalDPrime,
                alternative='two.sided',
                mu=0,
                exact=TRUE)
    
    #CALCULATE PERFORMANCE ON THE TWO PRESENTATIONS OF REPEATED TRIALS
    firstResponses = c() #stores trial accuracy (0 or 1) on first presentations
    secondResponses = c() #stores trial accuracy (0 or 1) on second presentations
    firstIndices = c() #stores trial index of first presentations
    secondIndices = c() #stores trial index of second presentations
    for (i in 1:length(allParticipantNumbers)) {
      participantPerformanceBlocksProbes = performanceBlocksProbes[performanceBlocksProbes$participantNumber == allParticipantNumbers[i],]
      firstIndex = c()
      secondIndex = c()
      listIndex = 1
      firstResponse = c()
      secondResponse = c()
      for (j in 1:nrow(participantPerformanceBlocksProbes)) {
        if (j %in% secondIndex) {
          #do nothing
        } else {
          letter1 = as.character(participantPerformanceBlocksProbes$leftProbe[j])
          letter2 = as.character(participantPerformanceBlocksProbes$rightProbe[j])
          index = 0
          for (k in (j+1):nrow(participantPerformanceBlocksProbes)) {
            letterSet = c(as.character(participantPerformanceBlocksProbes$leftProbe[k]),
                          as.character(participantPerformanceBlocksProbes$rightProbe[k]))
            if (letter1 %in% letterSet & letter2 %in% letterSet) {
              index = k
              break
            }
          }
          firstIndex = c(firstIndex, j)
          secondIndex = c(secondIndex, k)
          firstResponse = c(firstResponse, participantPerformanceBlocksProbes$participantCorrect[j])
          secondResponse = c(secondResponse, participantPerformanceBlocksProbes$participantCorrect[k])
        }
      }
      firstResponses = c(firstResponses, firstResponse)
      secondResponses = c(secondResponses, secondResponse)
      firstIndices = c(firstIndices, firstIndex)
      secondIndices = c(secondIndices, secondIndex)
    }
  
    #prepare for plot of first vs. second presentation accuracy through time
    presentationDF = data.frame("fR" = c(firstResponses),
                                "sR" = c(secondResponses),
                                "fI" = c(firstIndices),
                                "sI" = c(secondIndices),
                                "participant" = rep(allParticipantNumbers, each=45))
    presentationDF$firstBlock = ceiling(presentationDF$fI/30)
    presentationDF$secondBlock = ceiling(presentationDF$sI/30)
    
    #get average performance by trial for presentation 1
    averageTrialPerformancesPresentation1 = rep(NaN, 90)
    for (i in 1:length(averageTrialPerformancesPresentation1)) {
      averageTrialPerformancesPresentation1[i] = mean(presentationDF[presentationDF$fI == i & !is.na(presentationDF$fR),]$fR)
    }
    averageTrialPerformancesPresentation1[is.nan(averageTrialPerformancesPresentation1)] = NA
    
    #get average performance by trial for presentation 2
    averageTrialPerformancesPresentation2 = rep(NaN, 90)
    for (i in 1:length(averageTrialPerformancesPresentation2)) {
      averageTrialPerformancesPresentation2[i] = mean(presentationDF[presentationDF$sI == i & !is.na(presentationDF$sR),]$sR)
    }
    averageTrialPerformancesPresentation2[is.nan(averageTrialPerformancesPresentation2)] = NA
    
    #rescale for model creation
    presentationDF$fI = presentationDF$fI/90
    presentationDF$sI = presentationDF$sI/90
    
    #PLOT FIRST VS. SECOND PRESENTATION ACCURACIES BY TRIAL INDEX
    png('presentations.png',width=6,height=6,units='in',res=500)
    par(mai=c(boma, loma, toma, roma))#, xpd=TRUE)
    plot(trialIndices,
         averageTrialPerformancesPresentation1,
         xlab='Trial Index',
         ylab='Accuracy',
         ylim=c(0,1),
         main='Learning By Trial Presentation',
         col='lightgrey',
         xlim=c(0,90),
         pch=20,
         cex=cex,
         font.main=font.main,
         lwd=lwd,
         cex.main=cex.main,
         cex.axis=cex.axis,
         cex.lab=cex.lab,
         axes=FALSE)
    axis(side = 1,
         at = c(0, 30, 60, 90),
         labels = c('', 30, 60, 90))
    axis(side = 2,
         at = c(0.0, 0.5, 1.0))
    points(trialIndices,
           averageTrialPerformancesPresentation2,
           pch=20,
           cex=cex,
           col='darkgrey')
    x = seq(0, 1, 0.00001)
    model = glmer(fR ~ fI + (fI||participant),
                  family=binomial(link='probit'),
                  data=presentationDF[!is.na(presentationDF$fR),]) #mixed-effects probit regression
    intercept = fixef(model)[[1]]
    effect = fixef(model)[[2]]
    lines(x*90,
          pnorm(intercept + (effect*x)),
          lwd=lwd,
          col='lightgrey')
    model = glmer(sR ~ sI + (sI||participant),
                  family=binomial(link='probit'),
                  data=presentationDF[!is.na(presentationDF$sR),]) #mixed-effects probit regression
    intercept = fixef(model)[[1]]
    effect = fixef(model)[[2]]
    lines(x*90,
          pnorm(intercept + (effect*x)),
          lwd=lwd,
          col='darkgrey')
    mtext('Block 2',
          1,
          line=0.5,
          at=15)
    mtext('Block 3',
          1,
          line=0.5,
          at=45)
    mtext('Block 4',
          1,
          line=0.5,
          at=75)
    legend('bottom',
           legend=c('1st presentation', '2nd presentation'),
           lty=c(1,1),
           pch=c(20,20),
           col=c('lightgrey', 'darkgrey'),
           cex=cex.leg)
    dev.off()
    
    #compare performance on first and second presentations to see if model fits are meaningfully different
    comparePresentationDF = data.frame('r' = c(presentationDF$fR, presentationDF$sR),
                                       'i' = c(presentationDF$fI, presentationDF$sI),
                                       'indicator' = c(rep(0, nrow(presentationDF)), rep(1, nrow(presentationDF))),
                                       'participant' = c(presentationDF$participant, presentationDF$participant))
    model = glmer(r ~ i + indicator + i*indicator + (i + indicator + i*indicator||participant),
                  family=binomial(link='probit'),
                  data=comparePresentationDF[!is.na(comparePresentationDF$r),])
    print(paste("Combined Sample p-value for intercept difference, first vs. second presentations:", summary(model)[[10]][15]))
    print(paste("Combined Sample p-value for slope difference, first vs. second presentations:", summary(model)[[10]][16]))
    
    #compare performance on first and second presentations with Bayes factors
    n = set_prior('normal(0,1)', class = 'b')
    m = brm(r ~ i + indicator + i*indicator + (i + indicator + i*indicator||participant),
            family=binomial(link='probit'),
            data=comparePresentationDF[!is.na(comparePresentationDF$r),],
            chains=4,
            iter=2000,
            prior=n,
            sample_prior=TRUE)
    h1 = hypothesis(m, 'indicator=0')
    h2 = hypothesis(m, 'i:indicator=0')
    print(paste("Combined Sample evidence ratio for same intercepts, first vs. second presentations:", h1[[1]][[6]]))
    print(paste("Combined Sample evidence ratio for same slopes, first vs. second presentations:", h2[[1]][[6]]))
    
    #prepare performance by lag to most recent jump
    stepsSinceJump = numeric(nrow(performanceBlocks))
    for (i in 1:nrow(performanceBlocks)) {
      if (performanceBlocks$probe[i] == 0) {
        stepsSinceJump[i] = NA
      } else {
        inc = 1
        probes = 0
        j = FALSE
        while (j == FALSE) {
          wasJump = performanceBlocks$jump[i-inc]
          if (wasJump == 0) {
            inc = inc + 1
          } else if (performanceBlocks$block[i-inc] != performanceBlocks$block[i]) {
            j = TRUE
          } else if (wasJump == 1) {
            j = TRUE
          } else {
            inc = inc + 1
          }
          if (inc > 10) {
            break
          }
        }
        if (inc > 10) {
          stepsSinceJump[i] = 0
        } else {
          stepsSinceJump[i] = inc
        }
      }
    }
    performanceBlocks$stepsSinceJump = stepsSinceJump
    performanceBlocksProbes$stepsSinceJump = stepsSinceJump[which(!is.na(stepsSinceJump))]
    performanceBlocksProbes[performanceBlocksProbes$stepsSinceJump == 0,]$stepsSinceJump = NA 
    accuracyByLag = numeric(nrow(performanceBlocksProbes))
    for (i in 1:length(accuracyByLag)) {
      accuracyByLag[i] = mean(performanceBlocksProbes[performanceBlocksProbes$stepsSinceJump == performanceBlocksProbes$stepsSinceJump[i] & !is.na(performanceBlocksProbes$participantCorrect),]$participantCorrect)
    }
    
    #PLOT PERFORMANCE BY LAG TO THE MOST RECENT JUMP
    png("accuracyByJumpLag.png",width=6,height=6,units='in',res=500)
    par(mai=c(boma, loma, toma, roma))
    plot(performanceBlocksProbes$stepsSinceJump,
         accuracyByLag,
         pch=20,
         cex=cex,
         font.main=font.main,
         lwd=lwd,
         cex.main=cex.main,
         cex.axis=cex.axis,
         cex.lab=cex.lab,
         col='grey',
         main='Accuracy by Jump Recency',
         xlim=c(4, 7),
         ylim=c(0,1),
         axes=FALSE,
         ylab='Accuracy',
         xlab='Lag Since Jump')
    axis(side = 1,
         at=c(4,5,6,7))
    axis(side = 2,
         at=c(0,0.5,1))
    x = seq(4, 7, 0.001)
    for (i in 1:length(allParticipantNumbers)) {
      model = glm(participantCorrect ~ stepsSinceJump,
                  family=binomial(link='probit'),
                  data=performanceBlocksProbes[performanceBlocksProbes$participantNumber == allParticipantNumbers[i] & !is.na(performanceBlocksProbes$participantCorrect),])
      intercept = model[[1]][[1]]
      effect = model[[1]][[2]]
      lines(x,
            pnorm(intercept + (effect*x)),
            lty=1,
            lwd=lwdThin,
            col='grey')
    }
    model = glmer(participantCorrect ~ stepsSinceJump + (stepsSinceJump||participantNumber),
                  family=binomial(link='probit'),
                  data=performanceBlocksProbes[!is.na(performanceBlocksProbes$participantCorrect),])
    intercept = fixef(model)[[1]]
    effect = fixef(model)[[2]]
    lines(x,
          pnorm(intercept + (effect*x)),
          lty=1,
          lwd=lwd,
          col='black')
    legend('bottomright',
           legend=c('Group trend', 'Individual trend'),
           lty=c(1,1),
           col=c('black', 'grey'),
           cex=cex.leg)
    dev.off()
    
    #prepare to plot relative recencies of targets and lures
    targetRecencies = c()
    lureRecencies = c()
    participantNumbersForRecencies = c()
    for (i in 1:nrow(performanceBlocks)) {
      if (performanceBlocks$probe[i] == 1) {
        participantNumberForRecency = performanceBlocks$participantNumber[i]
        correctResponse = performanceBlocks$correctResponse[i]
        target = ''
        lure = ''
        if (correctResponse == 'left') {
          target = performanceBlocks$leftProbe[i]
          lure = performanceBlocks$rightProbe[i]
        } else if (correctResponse == 'right') {
          target = performanceBlocks$rightProbe[i]
          lure = performanceBlocks$leftProbe[i]
        } else {
          print('Error!')
        }
        targetRecency = 0
        lureRecency = 0
        targetFound = FALSE
        lureFound = FALSE
        j = 1
        while (targetFound == FALSE || lureFound == FALSE) {
          letter = performanceBlocks$letter[i-j]
          if (letter == target) {
            targetRecency = j
            targetFound = TRUE
          } else if (letter == lure) {
            lureRecency = j
            lureFound = TRUE
          }
          j = j + 1
        }
        targetRecencies = c(targetRecencies, targetRecency)
        lureRecencies = c(lureRecencies, lureRecency)
        participantNumbersForRecencies = c(participantNumbersForRecencies, participantNumberForRecency)
      }
    }
    medianRelativeRecencies = c()
    for (i in 1:length(allParticipantNumbers)) {
      participantRelativeRecencies = c()
      for (j in 1:length(participantNumbersForRecencies)) {
        if (participantNumbersForRecencies[j] == allParticipantNumbers[i]) {
          participantRelativeRecencies = c(participantRelativeRecencies, targetRecencies[j] - lureRecencies[j])
        }
      }
      medianRelativeRecencies = c(medianRelativeRecencies, median(participantRelativeRecencies))
    }
    print(paste("Relative recencies of targets and lures p-value:",
                wilcox.test(medianRelativeRecencies,
                    paired=FALSE,
                    mu=0,
                    alternative="two.sided",
                    exact=FALSE)[[3]]))
    
    #PLOT RELATIVE RECENCIES OF TARGETS AND LURES
    breaks = seq(-29.5, 29.5, 1)
    png('targetLureRecencies.png',width=12,height=6,units='in',res=500)
    par(mai=c(boma, loma, toma, roma))
    hist(targetRecencies-lureRecencies,
         breaks=breaks,
         main='Relative Recency of Probe Letters',
         xlab='Target Recency - Lure Recency',
         ylab='Frequency',
         ylim=c(0, 300),
         xlim=c(-30, 30),
         pch=20,
         cex=cex,
         font.main=font.main,
         lwd=lwd,
         lty=1,
         cex.main=cex.main,
         cex.axis=cex.main,
         cex.lab=cex.lab,
         col='grey',
         axes=FALSE)
    axis(side = 1,
         at = c(-30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30),
         labels = c(-30, '', -20, '', -10, '', 0, '', 10, '', 20, '', 30))
    axis(side = 2,
         at = c(0, 50, 100, 150, 200, 250, 300))
    dev.off()
    
    #get median reaction times of block 4 from good participants on correct trials
    medianRTs = matrix(data=NA, nrow=5, ncol=length(goodParticipantNumbers))
    medianRTsAll = matrix(data=NA, nrow=5, ncol=length(allParticipantNumbers))
    for (i in 1:length(goodParticipantNumbers)) {
      for (j in 2:6) {
        medianRTs[j-1, i] = median(block4Probes[block4Probes$participantNumber == goodParticipantNumbers[i] & block4Probes$minimumStepsToProbe == j & block4Probes$participantCorrect %in% c(1),]$reactionTime)
      }
    }
    for (i in 1:length(allParticipantNumbers)) {
      for (j in 2:6) {
        medianRTsAll[j-1, i] = median(block4Probes[block4Probes$participantNumber == allParticipantNumbers[i] & block4Probes$minimumStepsToProbe == j & block4Probes$participantCorrect %in% c(1),]$reactionTime)
      }
    }
    
    #get bootstrap CIs
    medianRTCIUpper = c()
    medianRTCILower = c()
    medianRTAllCIUpper = c()
    medianRTAllCILower = c()
    Tboot = rep(0, 100000)
    TbootAll = rep(0, 100000)
    for (j in 2:6) {
      for (b in 1:100000) {
        sample = sample(medianRTs[j-1,], length(medianRTs[j-1,]), replace=TRUE)
        Tboot[b] = mean(sample)
        sampleAll = sample(medianRTsAll[j-1,], length(medianRTsAll[j-1,]), replace=TRUE)
        TbootAll[b] = mean(sampleAll, na.rm=TRUE)
      }
      medianRTCIUpper = c(medianRTCIUpper, quantile(Tboot, 0.95)[[1]])
      medianRTCILower = c(medianRTCILower, quantile(Tboot, 0.05)[[1]])
      medianRTAllCIUpper = c(medianRTAllCIUpper, quantile(TbootAll, 0.95, na.rm=TRUE)[[1]])
      medianRTAllCILower = c(medianRTAllCILower, quantile(TbootAll, 0.05, na.rm=TRUE)[[1]])
    }
    
    #prepare more for RT analysis
    RTDF = data.frame("medianRT" = c(medianRTs),
                      "lag" = log(rep(c(2,3,4,5,6),length(goodParticipantNumbers))),
                      "participant" = rep(goodParticipantNumbers, each=5))
    block4Probes$logMinimumStepsToProbe = log(block4Probes$minimumStepsToProbe)
    
    #PLOT RESPONSE TIME DATA (HIGH PERFORMERS)
    png('goodRTs.png',width=6,height=6,units='in',res=500)
    par(mai=c(boma, loma, toma, roma))
    plot(jitter(log(rep(c(2,3,4,5,6),length(goodParticipantNumbers))), amount=0),
         c(medianRTs),
         main='Response Times by Lag\n(High Performers)',
         xlab='Log Lag to More Imminent Probe Letter',
         ylab='Response Time (s)',
         ylim=c(0, 3.5),
         xlim=c(log(2), log(6)),
         pch=20,
         cex=cex,
         font.main=font.main,
         lwd=lwd,
         lty=1,
         cex.main=cex.main,
         cex.axis=cex.main,
         cex.lab=cex.lab,
         col='grey',
         axes=FALSE)
    axis(side = 1,
         at = log(c(2,3,4,5,6)),
         labels=c(2,3,4,5,6))
    axis(side = 2,
         at = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5))
    x = seq(log(2), log(6), 0.001)
    for (i in 1:length(goodParticipantNumbers)) {
      model = lm(reactionTime ~ logMinimumStepsToProbe,
                 data=block4Probes[block4Probes$participantNumber == goodParticipantNumbers[i] & block4Probes$participantCorrect %in% c(1),])
      intercept = model[[1]][[1]]
      effect = model[[1]][[2]]
      lines(x,
            intercept + effect*x,
            lty=1,
            lwd=lwdThin,
            col='grey')
    }
    plotCI(log(c(2, 3, 4, 5, 6)),
           rowMeans(medianRTs),
           li=medianRTCILower,
           ui=medianRTCIUpper,
           add=TRUE,
           cex=cex,
           col='black',
           pch=20)
    model = lmer(reactionTime ~ logMinimumStepsToProbe + (logMinimumStepsToProbe||participantNumber),
                 data=block4Probes[block4Probes$participantNumber %in% goodParticipantNumbers & block4Probes$participantCorrect %in% c(1),])
    intercept = fixef(model)[[1]]
    effect = fixef(model)[[2]]
    lines(x,
          intercept + effect*x,
          lty=1,
          lwd=lwd)
    legend('bottomright',
           legend=c('Group trend', 'Individual trend'),
           pch=c(20,20),
           lty=c(1, 1),
           col=c('black', 'grey'),
           cex=cex.leg)
    dev.off()
    
    #PLOT RESPONSE TIME DATA (ALL PARTICIPANTS)
    png('allRTs.png',width=6,height=6,units='in',res=500)
    par(mai=c(boma, loma, toma, roma))
    plot(jitter(log(rep(c(2,3,4,5,6),length(allParticipantNumbers))), amount=0),
         c(medianRTsAll),
         main='Response Times by Lag\n(All Participants)',
         xlab='Log Lag to More Imminent Probe Letter',
         ylab='Response Time (s)',
         ylim=c(0, 3.5),
         xlim=c(log(2), log(6)),
         pch=20,
         cex=cex,
         font.main=font.main,
         lwd=lwd,
         lty=1,
         cex.main=cex.main,
         cex.axis=cex.main,
         cex.lab=cex.lab,
         col='grey',
         axes=FALSE)
    axis(side = 1,
         at = log(c(2,3,4,5,6)),
         labels=c(2,3,4,5,6))
    axis(side = 2,
         at = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5))
    x = seq(log(2), log(6), 0.001)
    for (i in 1:length(allParticipantNumbers)) {
      model = lm(reactionTime ~ logMinimumStepsToProbe,
                 data=block4Probes[block4Probes$participantNumber == allParticipantNumbers[i] & block4Probes$participantCorrect %in% c(1),])
      intercept = model[[1]][[1]]
      effect = model[[1]][[2]]
      lines(x,
            intercept + effect*x,
            lty=1,
            lwd=lwdThin,
            col='grey')
    }
    plotCI(log(c(2, 3, 4, 5, 6)),
           rowMeans(medianRTsAll, na.rm=TRUE),
           li=medianRTAllCILower,
           ui=medianRTAllCIUpper,
           add=TRUE,
           cex=cex,
           col='black',
           pch=20)
    model = lmer(reactionTime ~ logMinimumStepsToProbe + (logMinimumStepsToProbe||participantNumber),
                 data=block4Probes[block4Probes$participantCorrect %in% c(1),])
    intercept = fixef(model)[[1]]
    effect = fixef(model)[[2]]
    lines(x,
          intercept + effect*x,
          lty=1,
          lwd=lwd)
    legend('bottomright',
           legend=c('Group trend', 'Individual trend'),
           pch=c(20,20),
           lty=c(1, 1),
           col=c('black', 'grey'),
           cex=cex.leg)
    dev.off()
    
    #create success and response time matrices
    averageSuccessByLags = matrix(data=NA, nrow=8, ncol=8)
    averageSuccessByLagsCount = matrix(data=NA, nrow=8, ncol=8)
    averageSuccessByLagsGood = matrix(data=NA, nrow=8, ncol=8)
    averageRTByLagsGood = matrix(data=NA, nrow=8, ncol=8)
    for (i in c(2,3,7,8)) {
      for (j in 4:6) {
        if (i == 2 | i == 3) {
          averageSuccessByLags[i,j] = mean(block4Probes[block4Probes$minimumStepsToProbe == i & block4Probes$maximumStepsToProbe == j & block4Probes$participantCorrect %in% c(0,1),]$participantCorrect)
          averageSuccessByLagsCount[i,j] = nrow(block4Probes[block4Probes$minimumStepsToProbe == i & block4Probes$maximumStepsToProbe == j & block4Probes$participantCorrect %in% c(0,1),])
          averageSuccessByLagsGood[i,j] = mean(block4Probes[block4Probes$minimumStepsToProbe == i & block4Probes$maximumStepsToProbe == j & block4Probes$participantCorrect %in% c(0,1) & block4Probes$participantNumber %in% goodParticipantNumbers,]$participantCorrect)
          averageRTByLagsGood[i,j] = median(block4Probes[block4Probes$minimumStepsToProbe == i & block4Probes$maximumStepsToProbe == j & block4Probes$participantCorrect %in% c(0,1) & block4Probes$participantNumber %in% goodParticipantNumbers,]$reactionTime)
        } else {
          averageSuccessByLags[i,j] = mean(block4Probes[block4Probes$maximumStepsToProbe == i & block4Probes$minimumStepsToProbe == j & block4Probes$participantCorrect %in% c(0,1),]$participantCorrect)
          averageSuccessByLagsCount[i,j] = nrow(block4Probes[block4Probes$maximumStepsToProbe == i & block4Probes$minimumStepsToProbe == j & block4Probes$participantCorrect %in% c(0,1),])
          averageSuccessByLagsGood[i,j] = mean(block4Probes[block4Probes$maximumStepsToProbe == i & block4Probes$minimumStepsToProbe == j & block4Probes$participantCorrect %in% c(0,1) & block4Probes$participantNumber %in% goodParticipantNumbers,]$participantCorrect)
          averageRTByLagsGood[i,j] = median(block4Probes[block4Probes$maximumStepsToProbe == i & block4Probes$minimumStepsToProbe == j & block4Probes$participantCorrect %in% c(0,1) & block4Probes$participantNumber %in% goodParticipantNumbers,]$reactionTime)
        }
      }
    }
    averageSuccessByLags = averageSuccessByLags[,4:6]
    averageSuccessByLagsCount = averageSuccessByLagsCount[,4:6]
    averageSuccessByLagsGood = averageSuccessByLagsGood[,4:6]
    averageRTByLagsGood = averageRTByLagsGood[,4:6]
    print("Combined Sample accuracy by lag pairing (high performers):")
    print(averageSuccessByLagsGood)
    print("Combined Sample accuracy by lag pairing (all participants):")
    print(averageSuccessByLags)
    
    #accuracy regressions
    targetOuterInner =  block4Probes$stepsToTarget %% 2
    lureOuterInner = block4Probes$stepsToLure %% 2
    lureEarlyLate = block4Probes$stepsToLure
    for (i in 1:length(targetOuterInner)) {
      if (targetOuterInner[i] == 1) {
        targetOuterInner[i] = 'i'
      } else {
        targetOuterInner[i] = 'o'
      }
      if (lureOuterInner[i] == 1) {
        lureOuterInner[i] = 'i'
      } else {
        lureOuterInner[i] = 'o'
      }
      if (lureEarlyLate[i] %in% c(2,3)) {
        lureEarlyLate[i] = 'e'
      } else {
        lureEarlyLate[i] = 'l'
      }
    }
    block4Probes$targetOuterInner = factor(targetOuterInner)
    block4Probes$lureOuterInner = factor(lureOuterInner)
    block4Probes$lureEarlyLate = factor(lureEarlyLate)
    model = glmer(participantCorrect ~ targetOuterInner + lureOuterInner + lureEarlyLate + (targetOuterInner + lureOuterInner + lureEarlyLate||participantNumber),
                  family=binomial(link='probit'),
                  data=block4Probes[!is.na(block4Probes$participantCorrect),],
                  control=glmerControl(optimizer="bobyqa")) #mixed-effects probit regression
    print(paste("Combined Sample all participants effect of target (outer vs. inner) on accuracy p-value:", summary(model)[[10]][[14]]))
    print(paste("Combined Sample all participants effect of lure (outer vs. inner) on accuracy p-value:", summary(model)[[10]][[15]]))
    print(paste("Combined Sample all participants effect of lure (early vs. late) on accuracy p-value:", summary(model)[[10]][[16]]))
    model = glmer(participantCorrect ~ targetOuterInner + lureOuterInner + lureEarlyLate + (targetOuterInner + lureOuterInner + lureEarlyLate||participantNumber),
                  family=binomial(link='probit'),
                  data=block4Probes[!is.na(block4Probes$participantCorrect) & block4Probes$participantNumber %in% goodParticipantNumbers,],
                  control=glmerControl(optimizer="bobyqa")) #mixed-effects probit regression
    print(paste("Combined Sample high performers effect of target (outer vs. inner) on accuracy p-value:", summary(model)[[10]][[14]]))
    print(paste("Combined Sample high performers effect of lure (outer vs. inner) on accuracy p-value:", summary(model)[[10]][[15]]))
    print(paste("Combined Sample high performers effect of lure (early vs. late) on accuracy p-value:", summary(model)[[10]][[16]]))
    
    #calculate different scanning models
    block4Probes$logMinimumStepsToProbe = log(block4Probes$minimumStepsToProbe)
    modelLinearAll = lmer(reactionTime ~ minimumStepsToProbe + (minimumStepsToProbe||participantNumber),
                          data=block4Probes[block4Probes$participantCorrect %in% c(1),],
                          REML=FALSE)
    modelCompressedAll = lmer(reactionTime ~ logMinimumStepsToProbe + (logMinimumStepsToProbe||participantNumber),
                              data=block4Probes[block4Probes$participantCorrect %in% c(1),],
                              REML=FALSE)
    modelLinearGood = lmer(reactionTime ~ minimumStepsToProbe + (minimumStepsToProbe||participantNumber),
                           data=block4Probes[block4Probes$participantNumber %in% goodParticipantNumbers & block4Probes$participantCorrect %in% c(1),],
                           REML=FALSE)
    modelCompressedGood = lmer(reactionTime ~ logMinimumStepsToProbe + (logMinimumStepsToProbe||participantNumber),
                               data=block4Probes[block4Probes$participantNumber %in% goodParticipantNumbers & block4Probes$participantCorrect %in% c(1),],
                               REML=FALSE)
    print(paste("Combined Sample p-value for effect of lag on response time (all participants):", pt(-summary(modelLinearAll)[[10]][[6]], 24)*2))
    print(paste("Combined Sample p-value for effect of log lag on response time (all participants):", pt(-summary(modelCompressedAll)[[10]][[6]], 24)*2))
    print(paste("Combined Sample p-value for effect of lag on response time (high performers):", pt(-summary(modelLinearGood)[[10]][[6]], 7)*2))
    print(paste("Combined Sample p-value for effect of log lag on response time (high performers):", pt(-summary(modelCompressedGood)[[10]][[6]], 7)*2))
    
    #calculate scanning model accounting for further probe letter
    block4Probes$logMaximumStepsToProbe = log(block4Probes$maximumStepsToProbe)
    modelMaxAll = lmer(reactionTime ~ logMinimumStepsToProbe + logMaximumStepsToProbe + (logMinimumStepsToProbe + logMaximumStepsToProbe||participantNumber),
                       data=block4Probes[block4Probes$participantCorrect %in% c(1),],
                       REML=FALSE)
    print(paste("Combined Sample p-value for effect of just log lag to closest probe letter on response time (all participants):", pt(-summary(modelMaxAll)[[10]][[8]], 24)*2))
    print(paste("Combined Sample p-value for effect of just log lag to furthest probe letter on response time (all participants):", pt(-summary(modelMaxAll)[[10]][[9]], 24)*2))
    print(paste("Delta-AIC for log min model over log min and log max model:", AIC(modelCompressedAll, modelMaxAll)[2][[1]][2] - AIC(modelCompressedAll, modelMaxAll)[2][[1]][1]))
    modelMaxGood = lmer(reactionTime ~ logMinimumStepsToProbe + logMaximumStepsToProbe + (logMinimumStepsToProbe + logMaximumStepsToProbe||participantNumber),
                        data=block4Probes[block4Probes$participantNumber %in% goodParticipantNumbers & block4Probes$participantCorrect %in% c(1),],
                        REML=FALSE)
    print(paste("Combined Sample p-value for effect of just log lag to closest probe letter on response time (high performers):", pt(-summary(modelMaxGood)[[10]][[8]], 7)*2))
    print(paste("Combined Sample p-value for effect of just log lag to furthest probe letter on response time (high performers):", pt(-summary(modelMaxGood)[[10]][[9]], 7)*2))
    print(paste("Delta-AIC for log min model over log min and log max model:", AIC(modelCompressedGood, modelMaxGood)[2][[1]][2] - AIC(modelCompressedGood, modelMaxGood)[2][[1]][1]))
    corTestLag = cor.test(performanceBlocksProbes$minimumStepsToProbe,
                          performanceBlocksProbes$maximumStepsToProbe,
                          method='spearman',
                          exact=FALSE)
    print(paste('Combined Sample correlation value between more imminent and less imminent probe letter:', corTestLag[[4]][[1]]))
    print(paste('Combined Sample correlation between more imminent and less imminent probe letter p-value:', corTestLag[[3]]))
    
    #Vuong's variance and closeness tests
    library(nonnest2)
    library(merDeriv)
    vcl = function(obj) vcov(obj, full=TRUE)
    vt = vuongtest(modelLinearAll, modelCompressedAll, vc1=vcl, vc2=vcl)
    print(paste("Combined Sample distinguishability of linear and logarithmic scanning models in all participants p-value:", vt[[2]]))
    print(paste("Combined Sample relative closeness of logarithmic over linear scanning models in all participants p-value:", vt[[4]][[2]]))
    vt = vuongtest(modelLinearGood, modelCompressedGood, vc1=vcl, vc2=vcl)
    print(paste("Combined Sample distinguishability of linear and logarithmic scanning models in high performers p-value:", vt[[2]]))
    print(paste("Combined Sample relative closeness of logarithmic over linear scanning models in high performers p-value:", vt[[4]][[2]]))

  }
  
}