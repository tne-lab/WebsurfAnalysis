#!/usr/local/bin/python3

# Written by Emily Koithan 10/2021
# Note: Script only returns results from first run of task (i.e., if the participant restarted the task, the trials completed after restarting will not be added to the data file).

# Aaron McInnes updated 10/2021
# Added a loop for each run so that all experimental sessions are saved
# Output stores how long into the wait time the participant quit
# Calculates choice and rating RTs, and category rankings

import pandas as pd
import numpy as np
import re
import os
#from trial_class import Trial

class Trial(object):
    def __init__(self, IDVec, Category = 'NA', Delays = 999, Choices = 999, Ratings = 999, AvgRatings = 999, ChoiceRTs = 999, RatingRTs = 999, Thetas = 999, TransitRT = 999, CatRank = 999, GrpVec = 'CN', SubTypeVec = 'CN', LoopN = 1, runID = 'NA', quitTime = 'NA'):
        self.LoopN = LoopN;
        self.IDVec = IDVec;
        self.GrpVec = GrpVec;
        self.SubTypeVec = SubTypeVec;
        self.runID = runID;
        self.Category = Category;
        self.Delays = Delays;
        self.Choices = Choices;
        self.Ratings = Ratings;
        self.AvgRatings = AvgRatings;
        self.ChoiceRTs = ChoiceRTs;
        self.RatingRTs = RatingRTs;
        self.Thetas = Thetas;
        self.TransitRT = TransitRT;
        self.CatRank = CatRank;
        self.quitTime = quitTime
    def to_list(self):
        trial_list = [self.LoopN, self.IDVec, self.runID, self.Category, self.Delays, self.Choices, self.Ratings, self.AvgRatings, self.ChoiceRTs, self.RatingRTs, self.Thetas, self.TransitRT, self.CatRank, self.quitTime]
        return trial_list
    
# Function to determine video type
def get_cat(payload_string):
    if 'Kittens' in payload_string:
        return 'Animal'
    if 'Bike Fail' in payload_string:
        return 'Fail'
    if 'Dance' in payload_string:
        return 'Dance'
    if 'Landscapes' in payload_string:
        return 'Landscape'
    else:
        return 'NA'

# Function to determine video rating (will change to regex later)
def get_rating(payload_string):
    if '5 stars' in payload_string:
        return 5
    if '4 stars' in payload_string:
        return 4
    if '3 stars' in payload_string:
        return 3
    if '2 stars' in payload_string:
        return 2
    if '1 star' in payload_string:
        return 1
    else:
        return 'NA'
    
# Import data file
original_data_file = '/Volumes/GoogleDrive/My Drive/Websurf/Data/RawData/C5_TMS_WebSurf_10_14_2021.xlsx'
original_data = pd.read_excel(original_data_file)
original_data = original_data.sort_values(by=['CLIENT_TIMESTAMP'])
savePath = "/Volumes/GoogleDrive/My Drive/Websurf/Data/Processed/"
os.chdir(savePath)
original_data.to_csv('Sorted_COMPAS_data.csv', index = False)

# Create list of participant IDs in ascending order
participant_ids = list(set(original_data['PARTICIPANT_NUMBER']))
participant_ids.sort()

dfP = []

# Seperate data for each id into new data frame and remove EEfRT data
for id in participant_ids:
    id_data = original_data[original_data['PARTICIPANT_NUMBER'] == id]
    Pid_WS_data = id_data[id_data['TASK_NAME'] == 'Web Surf Task']
    Pid_WS_data = Pid_WS_data.reset_index(drop=True)
    
    #loop over run sessions AM 10/18
    runIDs = list(set(Pid_WS_data['RUN_ID']))
    runIDs.sort()
    
    df_runs = []
    
    for run in runIDs:
        
            # Initialize list of trials and lists for ratings and delays accepted per catagory
        Trials = []
        landscapeRatings = []
        animalRatings = []
        danceRatings = []
        failRatings = []
        landscapeDelays = []
        animalDelays = []
        danceDelays = []
        failDelays = []

        
        id_WS_data = Pid_WS_data[Pid_WS_data['RUN_ID'] == run]
        id_WS_data = id_WS_data.reset_index(drop=True)
        # Find start(s) of experiment section
        exp_start = np.where(id_WS_data['ACTION_TYPE'] == 13)[0] #Find where the experimental trials started
        if np.array(exp_start).size == 0:
            continue
        exp_start = int(exp_start) + 1
        
                #Don't process data if participant timed out of the experiment
        if (id_WS_data['ACTION_TYPE'] == 35).any():
            continue
        
        #Get category rankings       
        rankingsCats = []
        rankingsNumbers = []
        rankingInd = id_WS_data.loc[id_WS_data['ACTION_TYPE'] == 31]
        rankingInd = rankingInd.iloc[(len(rankingInd)-4):len(rankingInd)] #get final rankings 
        if rankingInd.empty:
            continue #break if didn't finish task
        
        for rank in range(len(rankingInd)):
            curRank = rankingInd.iloc[rank, 18]
            strSpl = curRank.split('"')
            rankingsCats.append(strSpl[3].split(' ')[1])
            rankingsNumbers.append(rank)
        rankings = np.vstack((rankingsCats, rankingsNumbers))
        #Matching rankings names to processed data output
        rankings[rankings == "Kittens"] = "Animal"
        rankings[rankings == "Bike"] = "Fail"
        rankings[rankings == "Landscapes"] = "Landscape"
        #store category rankings in dataframe
        rankings = pd.DataFrame(rankings, columns = rankings[[0],[0,1,2,3]])
        rankings = rankings.drop(labels=0, axis=0)
        
        
        # Iterates through rows of raw data and creates instances of the trial class, assigning attributes based on action type
        i = exp_start
        round = 1
        restart_number = 0
        trial_list = []
        new_trial = Trial(id) 
        while i < (len(id_WS_data.index)):
            new_trial.runID = run
            if (id_WS_data['ACTION_TYPE'][i] == 8): # Showing offer (marks start of new trial)
                new_trial = Trial(id)
                new_trial.LoopN = round
                delay = id_WS_data['PAYLOAD'][i] 
                delay = int(re.search(r'\d+', delay).group())
                new_trial.Delays = delay
            if (id_WS_data['ACTION_TYPE'][i] == 12): # Starting new round
                round = round + 1
            if (id_WS_data['ACTION_TYPE'][i] == 18): # Decided to skip 
                new_trial.Choices = 0
                category = get_cat(id_WS_data['PAYLOAD'][i])
                new_trial.Category = category
                new_trial.Ratings = 'NA'
                new_trial.RatingRTs = 'NA'
                Trials.append(new_trial)
            if (id_WS_data['ACTION_TYPE'][i] == 20): # Decided to stay
                new_trial.Choices = 1
                if (id_WS_data['ACTION_TYPE'][i + 1] == 32):
                    new_trial.Choices = 1 #Quits coded as a stay. Quits can be identified by a non-NA value in the quitTime column
                    new_trial.quitTime = id_WS_data['CLIENT_TIMESTAMP'][i+1] - id_WS_data['CLIENT_TIMESTAMP'][i]
                    new_trial.Ratings = 'NA'
                    new_trial.RatingRTs = 'NA'                    
                    Trials.append(new_trial)         
                else:
                    new_trial.quitTime = 'NA'
                category = get_cat(id_WS_data['PAYLOAD'][i])
                new_trial.Category = category    
                if new_trial.Category == 'Animal':
                    animalDelays.append(new_trial.Delays)
                if new_trial.Category == 'Fail':
                    failDelays.append(new_trial.Delays)
                if new_trial.Category == 'Landscape':
                    landscapeDelays.append(new_trial.Delays)
                if new_trial.Category == 'Dance':
                    danceDelays.append(new_trial.Delays)
            if (id_WS_data['ACTION_TYPE'][i] == 18) or (id_WS_data['ACTION_TYPE'][i] == 20): #get time of decision if decision made - AM 10/20/2021
                    decisionTimestamp = id_WS_data['CLIENT_TIMESTAMP'][i]
                    if (id_WS_data['ACTION_TYPE'][i-1] == 8): #Ensuring the last timestamp is an offer
                        offerTimestamp = id_WS_data['CLIENT_TIMESTAMP'][i-1]
                        timeDiff_Choice = decisionTimestamp - offerTimestamp
                        #timeDiff_Choice = timeDiff_Choice.total_seconds()*1000#gives RT in milliseconds
                    else:
                        timeDiff_Choice = "NA"
                    new_trial.ChoiceRTs = timeDiff_Choice  
            if (id_WS_data['ACTION_TYPE'][i] == 25): #Finished travelling
                transitEndTimestamp = id_WS_data['CLIENT_TIMESTAMP'][i]
                if (id_WS_data['ACTION_TYPE'][i-1] == 18) or (id_WS_data['ACTION_TYPE'][i-1] == 23): #Ensuring the last timestamp is a skip or rating
                    transitStartTimestamp = id_WS_data['CLIENT_TIMESTAMP'][i-1]
                elif (id_WS_data['ACTION_TYPE'][i-1] == 5):
                          if (id_WS_data['ACTION_TYPE'][i-2] == 18) or (id_WS_data['ACTION_TYPE'][i-2] == 23): #Ensuring the last timestamp is an offer
                              transitStartTimestamp = id_WS_data['CLIENT_TIMESTAMP'][i-2]
                timeDiff_Transit = transitEndTimestamp - transitStartTimestamp
                #timeDiff_Transit = timeDiff_Transit.total_seconds()*1000#gives RT in milliseconds
                new_trial.TransitRT = timeDiff_Transit      
            if (id_WS_data['ACTION_TYPE'][i] == 23): # Rated video 
                rating = get_rating(id_WS_data['PAYLOAD'][i])
                new_trial.Ratings = rating
                ratingTimestamp = id_WS_data['CLIENT_TIMESTAMP'][i]
                if (id_WS_data['ACTION_TYPE'][i-1] == 10): #Ensuring the last timestamp is the end of a video
                        endVidTimestamp = id_WS_data['CLIENT_TIMESTAMP'][i-1]
                        timeDiff_Rating = ratingTimestamp - endVidTimestamp
                        #timeDiff_Rating = timeDiff_Rating.total_seconds()*1000#gives RT in milliseconds
                elif (id_WS_data['ACTION_TYPE'][i-2] == 10): #Check two events back if last one isn't end of video
                        endVidTimestamp = id_WS_data['CLIENT_TIMESTAMP'][i-1]
                        timeDiff_Rating = ratingTimestamp - endVidTimestamp
                        #timeDiff_Rating = timeDiff_Rating.total_seconds()*1000#gives RT in milliseconds 
                else:
                        timeDiff_Rating = "NA"
                new_trial.RatingRTs = timeDiff_Rating    
                if new_trial.Category == 'Animal':
                    animalRatings.append(rating)
                if new_trial.Category == 'Fail':
                    failRatings.append(rating)
                if new_trial.Category == 'Landscape':
                    landscapeRatings.append(rating)
                if new_trial.Category == 'Dance':
                    danceRatings.append(rating)
                Trials.append(new_trial)
            i = i + 1
        
        # Adds AvgRating and Thetas attributes to each trial
        for trial in Trials:
            if trial.Category == 'Animal':
                if len(animalRatings) != 0:
                    trial.AvgRatings = sum(animalRatings)/len(animalRatings)
                else:
                    trial.AvgRatings = 'NA'
                if len(animalDelays) != 0:
                    trial.Thetas = sum(animalDelays)/len(animalDelays)
                else:
                    trial.Thetas = '0'
            if trial.Category == 'Fail':
                if len(failRatings) != 0:
                    trial.AvgRatings = sum(failRatings)/len(failRatings)
                else:
                    trial.AvgRatings = 'NA'
                if len(failDelays) != 0:
                    trial.Thetas = sum(failDelays)/len(failDelays)
                else:
                    trial.Thetas = '0'
            if trial.Category == 'Dance':
                if len(danceRatings) != 0:
                    trial.AvgRatings = sum(danceRatings)/len(danceRatings)
                else:
                    trial.AvgRatings = 'NA'
                if len(danceDelays) != 0:
                    trial.Thetas = sum(danceDelays)/len(danceDelays)
                else:
                    trial.Thetas = '0'
            if trial.Category == 'Landscape':
                if len(landscapeRatings) != 0:
                    trial.AvgRatings = sum(landscapeRatings)/len(landscapeRatings)
                else:
                    trial.AvgRatings = 'NA'
                if len(landscapeDelays) != 0:
                    trial.Thetas = sum(landscapeDelays)/len(landscapeDelays)
                else:
                    trial.Thetas = '0'
        
        # Delete duplicate trials and create list of lists containing the trials' attributes
        j = 1
        trial_list.append(Trials[0].to_list())
        while j < len(Trials):
            if (Trials[j] != Trials[j-1]):
                trial = Trials[j]
                trial = trial.to_list()
                trial_list.append(trial)
                j = j + 1
            else:
                j = j + 1
                      
        # Create data file for each participant and import Trial list of lists as dataframe
        df = pd.DataFrame(trial_list)
        #Add category rankings to data
        animalRank = rankings["Animal"]
        danceRank = rankings["Dance"]
        failRank = rankings["Fail"]
        landscapeRank = rankings["Landscape"]
        df.loc[df[3] == "Animal", 12] = animalRank[1]
        df.loc[df[3] == "Dance", 12] = danceRank[1]
        df.loc[df[3] == "Fail", 12] = failRank[1]
        df.loc[df[3] == "Landscape", 12] = landscapeRank[1]
        
        #save all runs for this participant
        df_runs.append(df)
        
    
    if len(df_runs) == 0: #Pass if no experimental runs completed for this participant
        continue    
    dfP = np.vstack(df_runs)
    dfP = pd.DataFrame(dfP)
    dfP.to_csv(str(id) + '_WS_data.csv', index = False, header = ['LoopN','IDVec', 'runID', 'Category','Delays','Choices','Ratings','AvgRatings','ChoiceRTs','RatingRTs','Thetas','TransitRT','CatRank', 'quitTime'])
        

    


    

    

        
