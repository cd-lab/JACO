#import relevant libraries
from psychopy import visual, core, event#, sound
import numpy
from numpy import random
from playsound import playsound
#import itertools

class JACOFunctions:
    
    def end(self):
        
        #get steps to target and lure
        targetStep = []
        lureStep = []
        for i in range(0, len(self.minimumStep)):
            if self.block[i] != 1:
                if self.minimumStep[i] in [4, 5, 6]:
                    targetStep.append(self.minimumStep[i])
                    lureStep.append(self.maximumStep[i])
                elif self.minimumStep[i] in [2, 3]:
                    targetStep.append(self.maximumStep[i])
                    lureStep.append(self.minimumStep[i])
                else:
                    targetStep.append(numpy.nan)
                    lureStep.append(numpy.nan)
            else:
                targetStep.append(self.minimumStep[i])
                lureStep.append(self.maximumStep[i])
        self.targetStep = targetStep
        self.lureStep = lureStep
    
    def generateProbes(self):
        
        #define pairs
        pairs = []
        lags = []
        for i in range(0, len(self.sequence)):
            probe1 = i
            for j in range(2, 5):
                probe2 = (i + j) % len(self.sequence)
                pairs.append((probe1, probe2))
                lags.append((probe2 - probe1) % 15)
        
        #create combos
        pairLagPositions = {2: {'ET': [-6, -5],
                                'LT': [-3, -2]},
                            3: {'ET': [-5, -4],
                                'LT': [-3, -2]},
                            4: {'ET': [-4],
                                'LT': [-2]}}
        allCombos = []
        index1 = 1
        index2 = 1
        flip = 0
        for i in range(0, len(pairs)):
            thisPair = pairs[i]
            thisLag = lags[i]
            thesePositions = pairLagPositions[thisLag]
            if thisLag == 4:
                allCombos.append(((thisPair[0] + thesePositions['ET'][0]) % 15, thisPair[0], thisPair[1]))
                allCombos.append(((thisPair[0] + thesePositions['LT'][0]) % 15, thisPair[0], thisPair[1]))
            else:
                if i in [len(pairs) - 2, len(pairs) - 3, len(pairs) - 5]:
                    allCombos.append(((thisPair[0] + thesePositions['ET'][(index1 + 1) % 2]) % 15, thisPair[0], thisPair[1]))
                    allCombos.append(((thisPair[0] + thesePositions['LT'][(index2) % 2]) % 15, thisPair[0], thisPair[1]))
                    flip = (flip + 1) % 2
                    if flip == 0:
                        index1 = (index1 + 1) % 2
                    elif flip == 1:
                        index2 = (index2 + 1) % 2
                else:
                    allCombos.append(((thisPair[0] + thesePositions['ET'][(index1) % 2]) % 15, thisPair[0], thisPair[1]))
                    allCombos.append(((thisPair[0] + thesePositions['LT'][(index2) % 2]) % 15, thisPair[0], thisPair[1]))
                    flip = (flip + 1) % 2
                    if flip == 0:
                        index1 = (index1 + 1) % 2
                    elif flip == 1:
                        index2 = (index2 + 1) % 2
        
        #prevent adjacencies
        random.shuffle(allCombos)
        
        #divide into blocks
        block2 = []
        block3 = []
        block4 = []
        for i in range(0, len(allCombos)):
            if i in numpy.arange(0, 30):
                block2.append(allCombos[i])
            elif i in numpy.arange(30, 60):
                block3.append(allCombos[i])
            elif i in numpy.arange(60, 90):
                block4.append(allCombos[i])
        
        #create block 1 lures
        def block1Lures(SLs, SPs):
            SLC = SLs
            SPC = SPs
            for i in range(0, len(SLC)):
                thisPosition = SPC[i]
                thisTarget = (SPC[i] + 1) % 15
                thisLure = SLC[i]
                if thisLure in [(thisPosition - 1) % 15, thisPosition, thisTarget, (thisTarget + 1) % 15]:
                    for k in range(0, len(SLC)):
                        if SLC[k] not in [(thisPosition - 1) % 15, thisPosition, thisTarget, (thisTarget + 1) % 15] and \
                        thisLure not in [(SPC[k] - 1) % 15, SPC[k], (SPC[k] + 1) % 15, (SPC[k] + 2) % 15]:
                            SLC[i] = SLC[k]
                            SLC[k] = thisLure
                            break
                else:
                    pass
            return SLC
        
        #execute block 1 creation    
        block1 = []
        samplePositions = numpy.arange(0, 15)
        random.shuffle(samplePositions)
        sampleLures = list(numpy.arange(0, 15))
        random.shuffle(sampleLures)
        finalLures = block1Lures(sampleLures, samplePositions)
        for i in range(0, len(finalLures)):
            block1.append((samplePositions[i], (samplePositions[i] + 1) % 15, finalLures[i]))

        #add blocks to self
        return {1: block1,
                2: block2,
                3: block3,
                4: block4}

    def generateProbesAlt(self):
        
        #create empty lists to fill with combinations
        position = numpy.zeros(90)
        target = numpy.zeros(90)
        lure = numpy.zeros(90)
        gestalt = list()
        
        #intersection function
        def intersection(list1, list2):
            list3 = [value for value in list1 if value in list2]
            return list3
        
        #order function
        def order(someList):
            lureList = someList
            shuffledList1 = [0] * 15
            shuffledList2 = [0] * 15
            earlyLate = ([0] * 15) + ([1] * 15)
            random.shuffle(earlyLate)
            for i in range(0, 15):
                shuffledList1[i] = earlyLate[i]
                shuffledList2[i] = (earlyLate[i] + 1) % 2
            for i in range(0, 15):
                earlyOrLate = earlyLate[i]
                earlyLure = random.choice(intersection([1, 2], lureList))
                lateLure = random.choice(intersection([6, 7], lureList))
                lureList.remove(earlyLure)
                lureList.remove(lateLure)
                if earlyOrLate == 0:
                    shuffledList1[i] = earlyLure
                    shuffledList2[i] = lateLure
                elif earlyOrLate == 1:
                    shuffledList1[i] = lateLure
                    shuffledList2[i] = earlyLure
                else:
                    print('Error!')
            return shuffledList1 + shuffledList2
                        
        #create all possibilities, though unshuffled
        positions = [i for i in numpy.arange(0, 15)] * 6
        targets = ([3] * 30) + ([4] * 30) + ([5] * 30)
        lures = order(([1] * 15) + ([6] * 7) + ([7] * 7) + [random.choice([6, 7])]) + \
                order(([1] * 7) + ([2] * 7) + ([6] * 7) + ([7] * 7) + [random.choice([1, 2]), random.choice([6, 7])]) + \
                order(([1] * 7) + ([2] * 7) + ([7] * 15) + [random.choice([1, 2])])
        earlyLateLures = []
        for i in range(0, len(lures)):
            if lures[i] in [1, 2]:
                earlyLateLures.append(0)
            elif lures[i] in [6, 7]:
                earlyLateLures.append(1)
            else:
                print('Error!')
        
        #shuffle combinations and and track duplicates
        firstTimeIndex = 0
        positionTarget = [0 for x in range(0, 45)]
        positionTargetIndex1 = [0 for x in range(0, 45)]
        positionTargetIndex2 = [0 for x in range(0, 45)]
        indices = random.choice(numpy.arange(0, len(positions)),
                                len(positions),
                                replace=False)
        for i in range(0, len(position)):
            j = indices[i]
            thisPosition = positions[j]
            thisTarget = targets[j]
            thisLure = lures[j] 
            if (thisPosition, thisTarget) in positionTarget:
                positionTargetIndex = numpy.nan
                for k in range(0, len(positionTarget)):
                    if positionTarget[k] == (thisPosition, thisTarget):
                        positionTargetIndex = k
                positionTargetIndex2[positionTargetIndex] = i
            elif (thisPosition, thisTarget) not in positionTarget:
                positionTarget[firstTimeIndex] = (thisPosition, thisTarget)
                positionTargetIndex1[firstTimeIndex] = i
                firstTimeIndex += 1
            position[i] = thisPosition
            target[i] = thisTarget
            lure[i] = thisLure
            gestalt.append((thisPosition, thisTarget, thisLure))
            
        #create block 2
        usedIndices = []
        allBlocks = []
        block2 = []
        positionSums = {0: 0,
                        1: 0,
                        2: 0,
                        3: 0,
                        4: 0,
                        5: 0,
                        6: 0,
                        7: 0,
                        8: 0,
                        9: 0,
                        10: 0,
                        11: 0,
                        12: 0,
                        13: 0,
                        14: 0}
        for i in range(0, len(position)):
            if i not in positionTargetIndex2:
                if positionSums[position[i]] < 2:
                    block2.append(gestalt[i])
                    allBlocks.append(gestalt[i])
                    usedIndices.append(i)
                    positionSums[position[i]] += 1
            if len(block2) == 30:
                break
            
        #create block 3
        block3 = []
        for i in range(0, len(usedIndices)):
            duplicateIndex = positionTargetIndex2[positionTargetIndex1.index(usedIndices[i])]
            block3.append(gestalt[duplicateIndex])
            allBlocks.append(gestalt[duplicateIndex])
            usedIndices.append(duplicateIndex)
            
        #create block 4
        block4 = []
        for i in range(0, len(gestalt)):
            if gestalt[i] not in allBlocks:
                block4.append(gestalt[i])
                usedIndices.append(i)
                
        #shuffle
        random.shuffle(block2)
        random.shuffle(block3)
        random.shuffle(block4)
        allBlocks = block2 + block3 + block4
        
        #create block 1 lures - shockingly hard for me
        def block1Lures(SLs, SPs):
            SLC = SLs
            SPC = SPs
            for i in range(0, len(SLC)):
                thisPosition = SPC[i]
                thisTarget = (SPC[i] + 1) % 15
                thisLure = SLC[i]
                if thisLure in [(thisPosition - 1) % 15, thisPosition, thisTarget, (thisTarget + 1) % 15]:
                    for k in range(0, len(SLC)):
                        if SLC[k] not in [(thisPosition - 1) % 15, thisPosition, thisTarget, (thisTarget + 1) % 15] and \
                        thisLure not in [(SPC[k] - 1) % 15, SPC[k], (SPC[k] + 1) % 15, (SPC[k] + 2) % 15]:
                            SLC[i] = SLC[k]
                            SLC[k] = thisLure
                            break
                else:
                    pass
            return SLC
            
        #execute block 1 creation    
        block1 = []
        samplePositions = numpy.arange(0, 15)
        random.shuffle(samplePositions)
        sampleLures = list(numpy.arange(0, 15))
        random.shuffle(sampleLures)
        finalLures = block1Lures(sampleLures, samplePositions)
        for i in range(0, len(finalLures)):
            block1.append((samplePositions[i], (samplePositions[i] + 1) % 15, finalLures[i]))
            
        #add blocks to self
        return {1: block1,
                2: block2,
                3: block3,
                4: block4}
            
    def getBins(self):
        
        bin0 = [1]
        bin1 = [0, 1]
        bin2 = [0, 0, 1, 1]
        bin3 = [0, 0, 0, 1, 1, 1]
        bin4 = [0, 0, 0, 0, 1, 1, 1, 1]
        
        bins = {0: bin0,
                1: bin1,
                2: bin2,
                3: bin3,
                4: bin4}
    
        return bins
        
    def getBinColors(self):
        
        binColors = ['orange',
                     'yellowgreen',
                     'plum']
        random.shuffle(binColors)
        
        binColorsDict = {0: 'blueviolet',
                         1: binColors[0],
                         2: binColors[1],
                         3: 'gold',
                         4: binColors[2]}
        
        return binColorsDict
    
    def getProbeCombos(self):
        
        combosNeg1 = [(-1, -1, (0,2)),
                      (-1, -1, (0,3)),
                      (-1, -1, (0,4)),
                      (-1, -1, (0,5)),
                      (-1, -1, (0,6)),
                      (-1, -1, (0,7))]
        
        combos0 = [(0, 0, (1,3)),
                   (0, 0, (1,4)),
                   (0, 0, (1,5)),
                   (0, 0, (2,4)),
                   (0, 0, (2,5)),
                   (0, 0, (3,6)),
                   (0, 0, (3,7)),
                   (0, 0, (4,6)),
                   (0, 0, (4,7)),
                   (0, 0, (5,7))]
        
        combos1 = [(1, 2, (1,3)),
                   (1, 2, (1,3)),
                   (1, 2, (1,3)),
                   (1, 2, (1,3)),
                   (1, 3, (1,4)),
                   (1, 3, (1,5)),
                   (1, 3, (1,6)),
                   (1, 3, (1,7))]
        
        combos2 = [(2, 1, (1,3)),
                   (2, 1, (1,3)),
                   (2, 1, (1,3)),
                   (2, 1, (1,3)),
                   (2, 3, (2,4)),
                   (2, 3, (3,5)),
                   (2, 3, (2,6)),
                   (2, 3, (3,7))]
        
        combos3 = [(3, 1, (1,4)),
                   (3, 1, (1,5)),
                   (3, 1, (1,6)),
                   (3, 1, (1,7)),
                   (3, 2, (2,4)),
                   (3, 2, (3,5)),
                   (3, 2, (2,6)),
                   (3, 2, (3,7))]
        
        combos = [(1, 2, (1,3)),
                  (1, 2, (1,3)),
                  (1, 2, (1,3)),
                  (1, 2, (1,3)),
                  (1, 3, (1,4)),
                  (1, 3, (1,5)),
                  (1, 3, (1,6)),
                  (1, 3, (1,7)),
                  (2, 1, (1,3)),
                  (2, 1, (1,3)),
                  (2, 1, (1,3)),
                  (2, 1, (1,3)),
                  (2, 3, (2,4)),
                  (2, 3, (3,5)),
                  (2, 3, (2,6)),
                  (2, 3, (3,7)),
                  (3, 1, (1,4)),
                  (3, 1, (1,5)),
                  (3, 1, (1,6)),
                  (3, 1, (1,7)),
                  (3, 2, (2,4)),
                  (3, 2, (3,5)),
                  (3, 2, (2,6)),
                  (3, 2, (3,7))]
        
        binNeg1Combos = []
        binNeg1Combos.extend(combosNeg1)
        binNeg1Combos.extend(combosNeg1)
        binNeg1Combos.extend(combosNeg1)
        
        bin0Combos = []
        bin0Combos.extend(combos0)
        bin0Combos.extend(combos0)
        bin0Combos.extend(combos0)
        
        bin1Combos = []
        bin1Combos.extend(combos1)
        bin1Combos.extend(combos1)
        
        bin2Combos = []
        bin2Combos.extend(combos2)
        bin2Combos.extend(combos2)
        
        bin3Combos = []
        bin3Combos.extend(combos3)
        bin3Combos.extend(combos3)
        
        allCombos = []
        allCombos.extend(combos)
        allCombos.extend(combos)
        allCombos.extend(combos)
        
        random.shuffle(binNeg1Combos)
        random.shuffle(bin0Combos)
        random.shuffle(bin1Combos)
        random.shuffle(bin2Combos)
        random.shuffle(bin3Combos)
        random.shuffle(allCombos)
        
        comboDict = {-1: binNeg1Combos,
                     0: bin0Combos,
                     1: bin1Combos,
                     2: bin2Combos,
                     3: bin3Combos,
                     4: allCombos}
        
        return comboDict
    
    def getSequence(self):
        
        alphabet = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
        
        allStimuli = ['B', 'C', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'X', 'Z']
        letters = random.choice(allStimuli, 15, replace=False)
        thisSequence = []
        
        for i in range(0, len(letters)):
            thisSequence.append(letters[i])
        
        random.shuffle(thisSequence)
        
        for i in range(0, len(thisSequence)):
            iSub1Index = alphabet.index(thisSequence[(i-1) % len(thisSequence)])
            iIndex = alphabet.index(thisSequence[i])
            iPlus1Index = alphabet.index(thisSequence[(i+1) % len(thisSequence)])
            if abs(iIndex - iPlus1Index) == 1:
                for j in range(0, len(thisSequence)):
                    jIndex = alphabet.index(thisSequence[j])
                    if j != i:
                        jSub1Index = alphabet.index(thisSequence[(j-1) % len(thisSequence)])
                        jPlus1Index = alphabet.index(thisSequence[(j+1) % len(thisSequence)])
                        if abs(jSub1Index - iIndex) != 1 and abs(jPlus1Index - iIndex) != 1 and abs(iSub1Index - jIndex) != 1 and abs(iPlus1Index - jIndex) != 1:
                            thisSequence[i] = alphabet[jIndex]
                            thisSequence[j] = alphabet[iIndex]
                            break
            else:
                pass
        
        return thisSequence
    
    def getTrainingSequence(self):
        
#        allStimuliVowels = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
#        thisSequence = numpy.setdiff1d(allStimuliVowels, self.sequence)
#        random.shuffle(thisSequence)
#        thisSequence = numpy.append(thisSequence, [''])
        
        #return thisSequence
        return self.sequence
    
    def __init__(self,
                 myWin):
        
        #set data structures to store important data
        self.block = []
        self.stimulusTime = []
        self.isJump = []
        self.isProbe = []
        self.binOn = []
        self.letter = []
        self.reward = []
        self.sequenceIndex = []
        
        self.leftProbe = []
        self.rightProbe = []
        self.probeType = []
        self.binType = []
        self.binColor = []
        self.stepToLeftProbe = []
        self.stepToRightProbe = []
        self.minimumStep = []
        self.maximumStep = []
        self.participantAnswer = []
        self.correctAnswer = []
        self.participantCorrect = []
        self.reactionTime = []
        
        self.trainStimulusTime = []
        self.trainLetter = []
        self.trainSequenceIndex = []
        self.trainIsJump = []
        self.trainReactionTime = []
        self.trainKey = []
        
        #set variable data structures
        self.sequence = self.getSequence()
        self.trainingSequence = self.getTrainingSequence()
        self.allBins = self.getBins()
        self.allBinColors = self.getBinColors()
        self.probes = self.generateProbes()
        self.probeCombos = self.getProbeCombos()
        self.probeComboIndex = 0
        
        #create all of the PsychoPy visual stimuli
        self.instructionObject = visual.TextStim(myWin,
                                                 text='',
                                                 pos=(0, 0),
                                                 color='black',
                                                 opacity=0,
                                                 wrapWidth=25)
        self.instructionObject.setAutoDraw(True)
        self.imageInstructionObject = visual.ImageStim(myWin,
                                                       opacity=0)
        self.imageInstructionObject.setAutoDraw(True)
        
        self.alertBar = visual.TextStim(win=myWin,
                                        text='',
                                        pos=(0, 4),
                                        color='black',
                                        opacity=0)
        self.alertBar.setAutoDraw(True)
        self.lowAlertBar = visual.TextStim(win=myWin,
                                           text='',
                                           pos=(0, -4),
                                           color='black',
                                           opacity=0)
        self.lowAlertBar.setAutoDraw(True)
        
        self.bin = visual.Rect(win=myWin,
                               width=4,
                               height=4,
                               lineWidth=8,
                               pos=(0,0),
                               opacity=0,
                               autoDraw=True,
                               fillColor=None,
                               lineColor=None)
#        self.bin = visual.Circle(win=myWin,
#                                 radius=2,
#                                 lineWidth=8,
#                                 pos=(0,0),
#                                 opacity=0,
#                                 autoDraw=True,
#                                 fillColor=None,
#                                 lineColor=None)
        self.centerText = visual.TextStim(win=myWin,
                                          text='',
                                          pos=(0, 0),
                                          color='black',
                                          opacity=0)
        self.centerText.setAutoDraw(True)
        self.leftText = visual.TextStim(win=myWin,
                                        text='',
                                        pos=(-3, 0),#4),
                                        color='white',
                                        opacity=0)
        self.leftText.setAutoDraw(True)
        self.rightText = visual.TextStim(win=myWin,
                                         text='',
                                         pos=(3, 0),#4),
                                         color='white',
                                         opacity=0)
        self.rightText.setAutoDraw(True)
        
        self.mouse = event.Mouse(visible=False,
                                 newPos=None,
                                 win=myWin)
        
    def instructions(self,
                     myWin,
                     instructionText,
                     imageText=''):
        
        #clear past keys
        event.getKeys()
        
        #write instructions and draw them
        scalar = float(2)/3
        smallScalar = 0.35
        if instructionText != '' and imageText == '':
            self.instructionObject.opacity = 1
            self.instructionObject.text = instructionText     
        elif type(imageText) == str:
            self.imageInstructionObject.image = imageText
            self.imageInstructionObject.size *= scalar
            self.imageInstructionObject.opacity = 1
        else:
            self.instructionObject.pos = (-3,0)
            self.instructionObject.opacity = 1
            self.instructionObject.text = instructionText
            img1 = visual.ImageStim(myWin,
                                    pos=(12,6.5))
            img1.image = imageText[0]
            img1.size *= smallScalar
            img1.opacity = 1
            img1.setAutoDraw(True)
            img2 = visual.ImageStim(myWin,
                                    pos=(12,2.75))
            img2.image = imageText[1]
            img2.size *= smallScalar
            img2.opacity = 1
            img2.setAutoDraw(True)
            img3 = visual.ImageStim(myWin,
                                    pos=(12,-1))
            img3.image = imageText[2]
            img3.size *= smallScalar
            img3.opacity = 1
            img3.setAutoDraw(True)
        myWin.flip()

        #wait until participant is ready
        moveOn = False
        while (moveOn == False):
            keys = event.getKeys()
            if ('space' in keys):
                core.wait(0.2)
                moveOn = True
                
        #clear screen and update
        if instructionText != '' and imageText == '':
            self.instructionObject.opacity = 0
            self.instructionObject.text = ''    
        elif type(imageText) == str:
            self.imageInstructionObject.size /= scalar
            self.imageInstructionObject.opacity = 0
        else:
            self.instructionObject.pos = (0,0)
            self.instructionObject.opacity = 0
            self.instructionObject.text = ''
            img1.size /= smallScalar
            img2.size /= smallScalar
            img3.size /= smallScalar
            img1.opacity = 0
            img2.opacity = 0
            img3.opacity = 0
            
        myWin.flip()
        
    def jump(self,
             myWin,
             sequenceIndex,
             sequenceLength,
             trialIndex,
             block):
        
        #newIndex = (sequenceIndex + 1) % sequenceLength
        #while newIndex == (sequenceIndex + 1) % sequenceLength or newIndex == sequenceIndex:
        #    newIndex = random.randint(0, sequenceLength)
        
        jitter = random.choice([3, 4, 5, 6])
        newIndex = (self.probes[block][trialIndex][0] - jitter) % 15
        
        return newIndex
      
    def log(self,
            currentTime,
            sequenceIndex,
            jump,
            addTime,
            finalKey):
        
        self.trainStimulusTime.append(currentTime)
        self.trainLetter.append(self.sequence[sequenceIndex])
        self.trainSequenceIndex.append(sequenceIndex)
        self.trainIsJump.append(jump)
        self.trainReactionTime.append(addTime)
        self.trainKey.append(finalKey)
    
    def probe(self,
              myWin,
              currentTime,
              sequenceIndex,
              sequenceLength,
              binType,
              trialIndex,
              block):
    
        #clear past keys
        event.getKeys()
        
        #set timing
        probeClock = core.Clock()
        maxTime = 4
        
        #choose probe
        thisProbe = self.probeCombos[binType][self.probeComboIndex]
        thisBinType = thisProbe[0]
        probeType = str(binType) + 'v' + str(thisProbe[1])
#        lag1 = thisProbe[2][0]
#        lag2 = thisProbe[2][1]
#        self.probeComboIndex += 1
        lag1 = self.probes[block][trialIndex][1]
        lag2 = self.probes[block][trialIndex][2]
        
        #set letters
        letter1 = ''
        letter2 = ''
        next8 = []
        for i in range(sequenceIndex + 1, sequenceIndex + 1 + 8):
            next8.append(i % len(self.sequence))
        letter1 = lag1 #next8[(lag1]
        letter2 = lag2 #next8[(lag2]
#        if block == 1:
#            letter1 = lag1
#            letter2 = lag2
#        else:
#            letter1 = next8[lag1]
#            letter2 = next8[lag2]

        #shuffle and pick sides
        leftIndex = 0
        rightIndex = 0
        whichSide = random.uniform() < 0.5
        if whichSide:
            leftIndex = letter1
            rightIndex = letter2
        else:
            leftIndex = letter2
            rightIndex = letter1
        
        #alter window
        self.centerText.opacity = 0
        self.centerText.text = ''
        self.leftText.opacity = 1
        self.leftText.text = self.sequence[leftIndex]
        self.rightText.opacity = 1
        self.rightText.text = self.sequence[rightIndex]
        self.bin.opacity = 1
        self.bin.lineColor = self.allBinColors[thisBinType]
        #if thisBinType == -1:
        #    self.bin.pos = (0,0)
        #    self.leftText.pos = (-3,0)
        #    self.rightText.pos = (3,0)
        myWin.flip()
        
        #wait for response
        moveOn = False
        finalKey = ''
        while (moveOn == False):
            keys = event.getKeys()
            if 'left' in keys or 'right' in keys:
                moveOn = True
                finalKey = keys[0]
            elif probeClock.getTime() > maxTime:
                moveOn = True
        
        return probeClock.getTime(), sequenceIndex, leftIndex, rightIndex, finalKey, probeType, thisBinType

#    def probeNext(self,
#                  myWin,
#                  currentTime,
#                  sequenceIndex,
#                  sequenceLength):
#    
#        #clear past keys
#        event.getKeys()
#        
#        #set timing
#        probeClock = core.Clock()
#        maxTime = 4
#        
#        #alter window
#        self.centerText.opacity = 1
#        self.centerText.text = 'Please press the next letter.'
#        myWin.flip()
#        
#        #wait for response
#        moveOn = False
#        finalKey = ''
#        nextLetter = sequenceIndex + 1 % sequenceLength
#        while (moveOn == False):
#            keys = event.getKeys()
#            if len(keys) > 0:
#                moveOn = True
#                finalKey = keys[0]
#            elif probeClock.getTime() > maxTime:
#                moveOn = True
#        
#        return probeClock.getTime(), sequenceIndex, finalKey, nextLetter

    def record(self,
               block,
               currentTime,
               sequenceIndex,
               probe,
               jump,
               binOn,
               binType,
               lastIndex=0,
               leftIndex='',
               rightIndex='',
               probeType='',
               participantAnswer='',
               reactionTime=0):
        
        if probe:
            
            self.block.append(block)
            self.stimulusTime.append(currentTime)
            self.isProbe.append(int(probe))
            self.isJump.append(numpy.nan)
            self.binOn.append(numpy.nan)
            self.letter.append('')
            self.sequenceIndex.append(numpy.nan)
            self.reward.append(numpy.nan)
            
            self.leftProbe.append(self.sequence[leftIndex])
            self.rightProbe.append(self.sequence[rightIndex])
            self.probeType.append(probeType)
            self.binType.append(binType)
            self.binColor.append(self.allBinColors[binType])
            self.participantAnswer.append(participantAnswer)
            self.reactionTime.append(reactionTime)
            
            stepToLeft = leftIndex - lastIndex
            if stepToLeft <= 0:
                stepToLeft += len(self.sequence)
            stepToRight = rightIndex - lastIndex
            if stepToRight <= 0:
                stepToRight += len(self.sequence)
            self.stepToLeftProbe.append(stepToLeft)
            self.stepToRightProbe.append(stepToRight)

            minStep = min(stepToLeft, stepToRight)
            self.minimumStep.append(minStep)
            maxStep = max(stepToLeft, stepToRight)
            self.maximumStep.append(maxStep)
            
            if stepToLeft <= len(self.allBins[binType]) and self.allBins[binType][stepToLeft-1] == 1:
                self.correctAnswer.append('left')
                self.participantCorrect.append(int(participantAnswer == 'left'))
            elif stepToRight <= len(self.allBins[binType]) and self.allBins[binType][stepToRight-1] == 1:
                self.correctAnswer.append('right')
                self.participantCorrect.append(int(participantAnswer == 'right'))
            else:
                print('Something is wrong!')
    
        self.block.append(block)
        self.stimulusTime.append(currentTime + reactionTime)
        self.isProbe.append(int(False))
        self.isJump.append(int(jump))
        self.binOn.append(binOn)
        self.letter.append(self.sequence[sequenceIndex])
        self.reward.append(int(((self.leftText.opacity == 1 and self.centerText.text in self.leftText.text) or (self.rightText.opacity == 1 and self.centerText.text in self.rightText.text)) and (self.bin.pos[1] == 0) and (self.bin.opacity == 1)))
        self.sequenceIndex.append(sequenceIndex)
        
        self.leftProbe.append('')
        self.rightProbe.append('')
        self.probeType.append('')
        if binOn == 1:
            self.binType.append(binType)
            self.binColor.append(self.allBinColors[binType])
        else:
            self.binType.append(numpy.nan)
            self.binColor.append('')
        self.participantAnswer.append('')
        self.reactionTime.append(0)
        self.stepToLeftProbe.append(numpy.nan)
        self.stepToRightProbe.append(numpy.nan)
        self.minimumStep.append(numpy.nan)
        self.maximumStep.append(numpy.nan)
        self.correctAnswer.append(numpy.nan)
        self.participantCorrect.append(numpy.nan)
               
    def task(self,
             myWin,
             block,
             binType,
             taskLength,
             probeStart):
        
        #set counting variables
        sequenceIndex = random.randint(0, len(self.sequence))
        sequenceLength = len(self.sequence)
        stepsSinceProbe = len(self.sequence)
        stepsSinceJump = 0
        binStep = 1000
        finalKey = ''
        questionCount = 0
        leftIndex = 0
        rightIndex = 0
        thisBinType = binType
        
        #set timing variables
        timeDelay = 1.4 # 1.2
        separator = 0.2
        nextTimeDelay = timeDelay
        
        #set probabilities
        #probabilityJump = 0.05
        #probabilityProbe = 0.3
        
        #prepare window
        self.leftText.color = self.allBinColors[binType]
        self.rightText.color = self.allBinColors[binType]
        self.centerText.opacity = 1
        self.centerText.text = self.sequence[sequenceIndex]
        myWin.flip()
        
        #set timers
        globalClock = core.Clock()
        currentTime = globalClock.getTime()
        self.record(block,
                    currentTime,
                    sequenceIndex,
                    False,
                    False,
                    0,
                    thisBinType)
        
        #task implementation
        while currentTime < taskLength and questionCount <= len(self.probes[block]):# questionCount <= len(self.probeCombos[binType]):
            
            if currentTime > nextTimeDelay:
                
                separator = 0.2
                
                self.alertBar.opacity = 0
                self.alertBar.text = ''
                
                probe = 0 #random.uniform() < probabilityProbe
                jump = 0 #random.uniform() < probabilityJump
                if currentTime > probeStart and questionCount < len(self.probes[block]):
                    if self.probes[block][questionCount][0] == sequenceIndex:
                        probe = 1
                if stepsSinceProbe < 8:
                    probe = 0
                    if questionCount == len(self.probes[block]) and stepsSinceProbe >= 7:
                        questionCount += 1
                elif stepsSinceProbe == 8:
                    probe = 0
                    jump = 1
                if self.leftText.opacity == 1 or self.rightText.opacity == 1:
                    jump = 0
                
                if probe: # and currentTime > probeStart:
                    
                    timePassed, lastIndex, leftIndex, rightIndex, finalKey, probeType, thisBinType = self.probe(myWin, currentTime, sequenceIndex, sequenceLength, binType, questionCount, block)
                    nextTimeDelay += timePassed
                    
                    binStep = 1
                    questionCount += 1
                    
                    if jump:
                        sequenceIndex = self.jump(myWin, sequenceIndex, sequenceLength, questionCount, block)
                    else:
                        sequenceIndex = (sequenceIndex + 1) % sequenceLength
                    
                    if finalKey == 'left':
                        self.rightText.opacity = 0
                        self.rightText.text = ''
                        myWin.flip()
                        core.wait(separator)
                        nextTimeDelay += separator
                        if self.allBins[thisBinType][binStep-1] == 1:
                            self.bin.opacity = 1
                        else:
                            self.bin.opacity = 0
                        myWin.flip()
                    elif finalKey == 'right':
                        self.leftText.opacity = 0
                        self.leftText.text = ''
                        myWin.flip()
                        core.wait(separator)
                        nextTimeDelay += separator
                        if self.allBins[thisBinType][binStep-1] == 1:
                            self.bin.opacity = 1
                        else:
                            self.bin.opacity = 0
                        myWin.flip()
                    else:
                        self.leftText.opacity = 0
                        self.leftText.text = ''
                        self.rightText.opacity = 0
                        self.rightText.text = ''
                        self.bin.opacity = 0
                        self.bin.lineColor = None
                        self.alertBar.opacity = 1
                        self.alertBar.text = 'Respond faster!'
                        binStep = 1000
                    self.centerText.opacity = 1
                    self.centerText.text = self.sequence[sequenceIndex]
                    core.wait(separator/2)
                    nextTimeDelay += separator/2
                    myWin.flip()
                    
                    self.record(block,
                                currentTime, 
                                sequenceIndex, 
                                probe,
                                jump,
                                max(self.leftText.opacity, self.rightText.opacity),
                                binType=thisBinType,
                                lastIndex=lastIndex, 
                                leftIndex=leftIndex, 
                                rightIndex=rightIndex,
                                probeType=probeType,
                                participantAnswer=finalKey, 
                                reactionTime=timePassed)
                    
                else:
                    
                    if jump:
                        sequenceIndex = self.jump(myWin, sequenceIndex, sequenceLength, questionCount, block)
                    else:
                        sequenceIndex = (sequenceIndex + 1) % sequenceLength
                    self.centerText.opacity = 1
                    self.centerText.text = self.sequence[sequenceIndex]                    
                    myWin.flip()
                    
                    self.record(block,
                                currentTime, 
                                sequenceIndex, 
                                False, 
                                jump, 
                                max(self.leftText.opacity, self.rightText.opacity),
                                binType=thisBinType)
                
                nextTimeDelay += timeDelay
            
                if max(self.leftText.opacity, self.rightText.opacity) == 1:
                    
                    if ((self.leftText.opacity == 1 and self.centerText.text in self.leftText.text) or (self.rightText.opacity == 1 and self.centerText.text in self.rightText.text)) and (self.bin.pos[1] == 0) and (self.bin.opacity == 1):
                        playsound('ding.wav')
                        
                if probe:
                    stepsSinceProbe = 1
                else:
                    stepsSinceProbe += 1
                
                if jump:
                    stepsSinceJump = 0
                else:
                    stepsSinceJump += 1
                        
            elif currentTime > nextTimeDelay - separator:
                
                self.centerText.opacity = 0
                self.centerText.text = ''
                myWin.flip()
                
                core.wait(separator/2)
                
                if binStep >= len(self.allBins[thisBinType]):
                    self.leftText.opacity = 0
                    self.leftText.text = ''
                    self.rightText.opacity = 0
                    self.rightText.text = ''
                    self.bin.opacity = 0
                    self.bin.lineColor = None
                    #self.bin.pos = (0,4)
                    #self.leftText.pos = (-3,4)
                    #self.rightText.pos = (3,4)
                    binStep = 1000
                    myWin.flip()
                else:
                    binStep += 1
                    if self.allBins[thisBinType][binStep-1] == 1:
                        self.bin.opacity = 1
                        #self.bin.pos = (0,0)
                        #self.leftText.pos = (-3,0)                        
                        #self.rightText.pos = (3,0)
                    else:
                        self.bin.opacity = 0
                        #self.bin.pos = (0,4)
                        #self.leftText.pos = (-3,4)
                        #self.rightText.pos = (3,4)
                    myWin.flip()

                separator = -1000
            
            currentTime = globalClock.getTime()
            core.wait(0.005)
        
        #reset variables
        self.probeComboIndex = 0
        
        #clear all visual stimuli
        self.centerText.opacity = 0
        self.centerText.text = ''
        self.leftText.opacity = 0
        self.leftText.text = ''
        self.rightText.opacity = 0
        self.rightText.text = ''
        self.bin.opacity = 0
        self.bin.lineColor = None
        self.bin.fillColor = None
        myWin.flip()

    def training(self,
                 myWin,
                 binType,
                 requestedResponse):
        
        #set counting variables
        sequenceIndex = 0
        leftIndex = 0
        rightIndex = 0
        
        #set timing variables
        timeDelay = 1.2
        separator = 0.2
        nextTimeDelay = timeDelay
        
        #prepare window
        self.leftText.color = self.allBinColors[binType]
        self.rightText.color = self.allBinColors[binType]
        self.centerText.opacity = 1
        self.centerText.text = self.trainingSequence[0]
        myWin.flip()
        sequenceIndex += 1
        
        #set timers
        globalClock = core.Clock()
        currentTime = globalClock.getTime()

        #task implementation
        while sequenceIndex < 11:
            
            if currentTime > nextTimeDelay:
            
                if sequenceIndex in [1, 2]:
                
                    self.centerText.opacity = 1
                    self.centerText.text = self.trainingSequence[sequenceIndex]
                    myWin.flip()
                    
                elif sequenceIndex in [3]:
                    
                    leftIndex, rightIndex, timePassed = self.trainingProbe(myWin,
                                                                           binType,
                                                                           requestedResponse)
                    
                    if requestedResponse == 'left':
                        self.rightText.opacity = 0
                        self.rightText.text = ''
                    elif requestedResponse == 'right':
                        self.leftText.opacity = 0
                        self.leftText.text = ''
                    myWin.flip()
                    
                    core.wait(separator)
                    self.bin.opacity = 0
                    myWin.flip()
                    
                    core.wait(separator/2)
                    self.centerText.opacity = 1
                    self.centerText.text = self.trainingSequence[sequenceIndex]
                    myWin.flip()
                    
                    nextTimeDelay += timePassed + (separator*1.5)
                
                elif sequenceIndex in [4, 5]:
                     
                    self.centerText.opacity = 1
                    self.centerText.text = self.trainingSequence[sequenceIndex]
                    myWin.flip()
                    
                elif sequenceIndex in [6, 7, 8]:
                    
                    self.centerText.opacity = 1
                    self.centerText.text = self.trainingSequence[sequenceIndex]
                    myWin.flip()
                    
                    if ((self.leftText.opacity == 1 and self.centerText.text in self.leftText.text) or (self.rightText.opacity == 1 and self.centerText.text in self.rightText.text)) and (self.bin.pos[1] == 0) and (self.bin.opacity == 1):
                        
                        playsound('ding.wav')
                        self.lowAlertBar.opacity = 1
                        self.lowAlertBar.text = 'The letter you picked is in the square and you heard a sound!'
                        myWin.flip()
                        
                        core.wait(4)
                        nextTimeDelay += 4
                        
                        self.lowAlertBar.opacity = 0
                        self.lowAlertBar.text = ''
                        myWin.flip()
                        
                elif sequenceIndex in [9, 10]:
                    
                    self.centerText.opacity = 1
                    self.centerText.text = self.trainingSequence[sequenceIndex]
                    myWin.flip()
                    
                    if sequenceIndex == 10:
                        core.wait(timeDelay)
                
                nextTimeDelay += timeDelay
                sequenceIndex += 1
            
            elif currentTime > nextTimeDelay - separator:
                
                self.centerText.opacity = 0
                self.centerText.text = ''
                myWin.flip()
                
                if sequenceIndex == 6:
                    core.wait(separator/2)
                    self.bin.opacity = 1
                    myWin.flip()
                elif sequenceIndex == 9:
                    core.wait(separator/2)
                    self.bin.opacity = 0
                    self.leftText.opacity = 0
                    self.leftText.text = ''
                    self.rightText.opacity = 0
                    self.rightText.text = ''
                    myWin.flip()
            
            currentTime = globalClock.getTime()
            core.wait(0.005)
             
        #clear all visual stimuli
        self.centerText.opacity = 0
        self.centerText.text = ''
        self.leftText.opacity = 0
        self.leftText.text = ''
        self.rightText.opacity = 0
        self.rightText.text = ''
        self.bin.opacity = 0
        self.bin.lineColor = None
        myWin.flip()
            
    def trainingProbe(self,
                      myWin,
                      binType,
                      requestedResponse):
    
        #clear past keys
        event.getKeys()
        
        #set timing
        probeClock = core.Clock()
        
        #choose probe
        leftIndex = 5
        rightIndex = 7
        
        #alter window
        self.centerText.opacity = 0
        self.centerText.text = ''
        self.leftText.opacity = 1
        self.leftText.text = self.trainingSequence[leftIndex]
        self.rightText.opacity = 1
        self.rightText.text = self.trainingSequence[rightIndex]
        self.bin.opacity = 1
        self.bin.lineColor = self.allBinColors[binType]
        myWin.flip()
        
        #wait for response
        moveOn = False
        while (moveOn == False):
            keys = event.getKeys()
            if requestedResponse in keys:
                moveOn = True
                
        return leftIndex, rightIndex, probeClock.getTime()