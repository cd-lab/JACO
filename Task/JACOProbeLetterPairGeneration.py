#import libraries
import numpy
from numpy import random

#define sequence
allStimuli = ['B', 'C', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'X', 'Z']
letters = random.choice(allStimuli, 15, replace=False)
sequence = []
for i in range(0, len(letters)):
    sequence.append(letters[i])

#define pairs
pairs = []
lags = []
for i in range(0, len(sequence)):
    probe1 = i
    for j in range(2, 5):
        probe2 = (i + j) % len(sequence)
        pairs.append((probe1, probe2))
        lags.append((probe2 - probe1) % 15)

#create combos
targetLure = [(0, 0)] * len(pairs)
allPositions = [i for i in numpy.arange(0, 15)] * 6
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
#        elif i in [len(pairs) - 3, len(pairs) - 5]:
#            allCombos.append(((thisPair[0] + thesePositions['ET'][(index1 + 1) % 2]) % 15, thisPair[0], thisPair[1]))
#            allCombos.append(((thisPair[0] + thesePositions['LT'][(index2) % 2]) % 15, thisPair[0], thisPair[1]))
#            flip = (flip + 1) % 2
#            if flip == 0:
#                index1 = (index1 + 1) % 2
#            elif flip == 1:
#                index2 = (index2 + 1) % 2
        else:
            allCombos.append(((thisPair[0] + thesePositions['ET'][(index1) % 2]) % 15, thisPair[0], thisPair[1]))
            allCombos.append(((thisPair[0] + thesePositions['LT'][(index2) % 2]) % 15, thisPair[0], thisPair[1]))
            flip = (flip + 1) % 2
            if flip == 0:
                index1 = (index1 + 1) % 2
            elif flip == 1:
                index2 = (index2 + 1) % 2

#check position counts
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
for i in range(0, len(allCombos)):
    positionSums[allCombos[i][0]] += 1
    
#check position counts, early vs. late targets
positionSumsEL = {0: [0, 0],
                  1: [0, 0],
                  2: [0, 0],
                  3: [0, 0],
                  4: [0, 0],
                  5: [0, 0],
                  6: [0, 0],
                  7: [0, 0],
                  8: [0, 0],
                  9: [0, 0],
                  10: [0, 0],
                  11: [0, 0],
                  12: [0, 0],
                  13: [0, 0],
                  14: [0, 0]}
for i in range(0, len(allCombos)):
    earlyLag = (allCombos[i][1] - allCombos[i][0]) % 15
    if earlyLag in [4, 5, 6]:
        positionSumsEL[allCombos[i][0]][0] += 1
    else:
        positionSumsEL[allCombos[i][0]][1] += 1

#check letter sums, target vs. lure
letterSumsTL = {0: [0, 0],
                1: [0, 0],
                2: [0, 0],
                3: [0, 0],
                4: [0, 0],
                5: [0, 0],
                6: [0, 0],
                7: [0, 0],
                8: [0, 0],
                9: [0, 0],
                10: [0, 0],
                11: [0, 0],
                12: [0, 0],
                13: [0, 0],
                14: [0, 0]}
for i in range(0, len(allCombos)):
    position = allCombos[i][0]
    earlyLetter = allCombos[i][1]
    lateLetter = allCombos[i][2]
    earlyLag = (allCombos[i][1] - allCombos[i][0]) % 15
    if earlyLag in [4, 5, 6]:
        letterSumsTL[earlyLetter][0] += 1
        letterSumsTL[lateLetter][1] += 1
    else:
        letterSumsTL[earlyLetter][1] += 1
        letterSumsTL[lateLetter][0] += 1
    
#check letter sums, early vs. late
letterSumsEL = {0: [0, 0],
                1: [0, 0],
                2: [0, 0],
                3: [0, 0],
                4: [0, 0],
                5: [0, 0],
                6: [0, 0],
                7: [0, 0],
                8: [0, 0],
                9: [0, 0],
                10: [0, 0],
                11: [0, 0],
                12: [0, 0],
                13: [0, 0],
                14: [0, 0]}
for i in range(0, len(allCombos)):
    position = allCombos[i][0]
    earlyLetter = allCombos[i][1]
    lateLetter = allCombos[i][2]
    letterSumsEL[earlyLetter][0] += 1
    letterSumsEL[lateLetter][1] += 1
    
#check letter sums, target vs. lure AND early vs. late
letterSumsTLEL = {0: [[0, 0], [0, 0]],
                  1: [[0, 0], [0, 0]],
                  2: [[0, 0], [0, 0]],
                  3: [[0, 0], [0, 0]],
                  4: [[0, 0], [0, 0]],
                  5: [[0, 0], [0, 0]],
                  6: [[0, 0], [0, 0]],
                  7: [[0, 0], [0, 0]],
                  8: [[0, 0], [0, 0]],
                  9: [[0, 0], [0, 0]],
                  10: [[0, 0], [0, 0]],
                  11: [[0, 0], [0, 0]],
                  12: [[0, 0], [0, 0]],
                  13: [[0, 0], [0, 0]],
                  14: [[0, 0], [0, 0]]}
for i in range(0, len(allCombos)):
    position = allCombos[i][0]
    earlyLetter = allCombos[i][1]
    lateLetter = allCombos[i][2]
    earlyLag = (allCombos[i][1] - allCombos[i][0]) % 15
    if earlyLag in [4, 5, 6]:
        letterSumsTLEL[earlyLetter][0][0] += 1
        letterSumsTLEL[lateLetter][1][1] += 1
    else:
        letterSumsTLEL[earlyLetter][0][1] += 1
        letterSumsTLEL[lateLetter][1][0] += 1

#check matrix
matrix = numpy.zeros((8, 3))
for i in range(0, len(allCombos)):
    position = allCombos[i][0]
    earlyLetter = allCombos[i][1]
    lateLetter = allCombos[i][2]
    earlyLag = (allCombos[i][1] - allCombos[i][0]) % 15
    lateLag = (allCombos[i][2] - allCombos[i][0]) % 15
    if earlyLag in [4, 5, 6]:
        matrix[lateLag - 1, earlyLag - 1 - 3] += 1
    else:
        matrix[earlyLag - 1, lateLag - 1 - 3] += 1

#define possible positions
possibilities = []
for i in range(0, len(pairs)):
    thisInfo = pairLagPositions[lags[i]]
    for j in range(0, len(thisInfo['ET'])):
        possibilities.append(((pairs[i][0] + thisInfo['ET'][j]) % 15, pairs[i][0], pairs[i][1]))
    for j in range(0, len(thisInfo['LT'])):
        possibilities.append(((pairs[i][0] + thisInfo['LT'][j]) % 15, pairs[i][0], pairs[i][1]))