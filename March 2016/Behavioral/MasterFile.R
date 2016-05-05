# Master file

createStimLists.R
createFixLists.R
# used to randomly assign faces to stim lists in order to randomize which subjects see which
# stimuli in E-Prime. Ultimately not used, just saving code

dataReduction.R
# Takes relevant columns from Eprime files
# separates eval and cat data
# renames trials so that it goes 1...512
# adds column for trial type
# adds column for chunk (1-4)

Eval_RTanalysis.R
# looks at distributions of RTs, over all subs and within each sub
# makes figures for average RT in each condition (FaceRace x wordVal x FixArea)
# break down by chunk/block

Eval_ACCanalysis.R
# looks at overall accuracy for each subject
# makes figures for average accuracy in each condition 
# breaks down by half

