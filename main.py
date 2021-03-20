#!/usr/bin/env python3
import pandas as pd 
from minisom import MiniSom
import numpy as np 
from sklearn import preprocessing
import matplotlib.pyplot as plt 

df = pd.read_excel('Handball Woman European Data Set.xlsx')
df_players = df.groupby('Name').agg(['mean'])
df_players = df_players[['7mPGoals', '7mPMissed', '6mCGoals', '6mCMissed', 'WingGoals', 'WingMissed', 'BTGoals', 'BTMissed', 'FBGoals', 'FBMissed', 'FTOMissed', '9mGoals', '9mMissed', 'RC', '2M', '2+2', 'AS', 'R7', 'ST', 'BS', 'P7', 'TO']]

print(df_players.head())
normalized_df_players = preprocessing.normalize(df_players)

new_df = pd.DataFrame(normalized_df_players)
print(new_df)

som = MiniSom(6, 6, 22)
som.train(normalized_df_players, 100)

plt.figure(figsize=(6,6))
plt.pcolor(som.distance_map().T, cmap='bone_r')
plt.colorbar()
plt.show()
