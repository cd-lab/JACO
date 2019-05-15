#import relevant libraries
from psychopy import visual, gui
import pandas
import numpy
import math
from scipy.stats import binom_test

#import my library
import JACOFunctions

#get participant information
participantNumberDict = {'Participant Number':''}
dialogueBox = gui.DlgFromDict(dictionary=participantNumberDict)
participantNumber = str(participantNumberDict['Participant Number'])

#create a window
myWin = visual.Window(size=[1280,720], 
                      pos=[0,40],
                      monitor="testMonitor", 
                      units="deg",
                      fullscr=True)  

#instantiate class
data = JACOFunctions.JACOFunctions(myWin)

#INTRODUCTION
data.instructions(myWin, '', 'Inst0.png')
data.instructions(myWin, '', 'Inst1.png')

#SEQUENCE
text = 'Next you will learn one such sequence of letters. Pay attention: your knowledge of the sequence will allow you to earn more money.\n\nWhen two letters and a square appear on the screen, choose the next letter of the sequence using the LEFT ARROW and RIGHT ARROW keys.\n\nIf you choose correctly, you will hear a sound and earn a 15 cent reward. You will be paid what you earn.'
data.instructions(myWin, text)
text= 'When you are ready, press the SPACE BAR to begin.'
data.instructions(myWin, text)
data.task(myWin,
          block=1,
          binType=0,
          taskLength=780,
          probeStart=240)
text = 'Please pause and notify the experimenter that you have completed this block.'
data.instructions(myWin, text)

#BRIEF TRAINING
text = 'In the previous block, you were occasionally prompted to select the next letter of the sequence. The following blocks will ask you to select the letter that will occur during some interval in the future.\n\nIn these blocks, when you select a letter, the square will disappear for a short, fixed period while the sequence continues. The square will then reappear and remain for a short, fixed period while the sequence continues.\n\nYou should choose the letter that will appear in the sequence while the square will be on the screen. The sequence will not jump during this process.'
data.instructions(myWin, text)
text = 'You will complete a short example to learn how the square behaves. For this example, select the letter on the left.'
data.instructions(myWin, text)
data.training(myWin,
              binType=3,
              requestedResponse='left')
text = 'The letter you chose did not appear while the square was on the screen. That is why you did not hear a sound.\n\nYou will complete the same example again. Remember, you want to learn how the square behaves. This time, select the letter on the right.'
data.instructions(myWin, text)
data.training(myWin,
              binType=3,
              requestedResponse='right')

#FINAL TASK
text = 'You will now complete 3 blocks with the square from the example. Please ask any questions you have before proceeding.'
data.instructions(myWin, text)
text= 'When you are ready, press the SPACE BAR to begin.'
data.instructions(myWin, text)
data.task(myWin,
          block=2,
          binType=3,
          taskLength=780,
          probeStart=60)
text = 'Please pause and notify the experimenter that you have completed this block.'
data.instructions(myWin, text)
data.getProbeCombos()
text = 'You will now complete the next block. Please ask any questions you have before proceeding.'
data.instructions(myWin, text)
text= 'When you are ready, press the SPACE BAR to begin.'
data.instructions(myWin, text)
data.task(myWin,
          block=3,
          binType=3,
          taskLength=780,
          probeStart=60)
text = 'Please pause and notify the experimenter that you have completed this block.'
data.instructions(myWin, text)
data.getProbeCombos()
text = 'You will now complete the final block. Please ask any questions you have before proceeding.'
data.instructions(myWin, text)
text= 'When you are ready, press the SPACE BAR to begin.'
data.instructions(myWin, text)
data.task(myWin,
          block=4,
          binType=3,
          taskLength=780,
          probeStart=60)

#END
myWin.close()

#CALCULATE EARNINGS
earnings = 12 + float(numpy.nansum(data.reward)*0.15)
roundedEarnings = float(math.ceil(earnings*4))/4
print('You earned $' + str(roundedEarnings) + '!')

#FINAL CALCULATIONS
data.end()

#SAVE
subjectTaskData = pandas.DataFrame({'block':data.block,
                                    'stimulusTime':data.stimulusTime,
                                    'letter':data.letter,
                                    'letterIndex':data.sequenceIndex,
                                    'jump':data.isJump,
                                    'probe':data.isProbe,
                                    'bin':data.binOn,
                                    'binType':data.binType,
                                    'binColor':data.binColor,
                                    'leftProbe':data.leftProbe,     
                                    'rightProbe':data.rightProbe,
                                    'probeType':data.probeType,
                                    'stepsToTarget':data.targetStep,
                                    'stepsToLure':data.lureStep,
                                    'stepsToLeftProbe':data.stepToLeftProbe,
                                    'stepsToRightProbe':data.stepToRightProbe,
                                    'minimumStepsToProbe':data.minimumStep,
                                    'maximumStepsToProbe':data.maximumStep,
                                    'reactionTime':data.reactionTime,
                                    'participantResponse':data.participantAnswer,
                                    'correctResponse':data.correctAnswer,
                                    'participantCorrect':data.participantCorrect,
                                    'rewardEarned':data.reward})

subjectTaskData.to_csv(participantNumber + 'Task.csv', index=False)