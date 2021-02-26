import numpy as np
import pandas as pd

class TimeSeriesFeatures():

    def __init__(self, file_ts:str, file_ma:str):
        
        time_series = np.recfromtxt(file_ts)
        self.time_series = np.reshape(time_series, (1, time_series.size))
        ma_part = np.recfromtxt(file_ma)
        self.ma_part = np.reshape(ma_part, (1, ma_part.size))
    
    def create_feature_matrix(self, ar:int, ma:int):
        
        max_par = ar if ar > ma else ma
        len_matr = self.time_series.size - max_par

        feature_matrix = pd.DataFrame({"intercept":np.ones((1,len_matr))[0]}, index = range(len_matr))
        for i in range(1, ar+1):
            feature_matrix[f"y(k-{i})"] = self.time_series[0, max_par-i:-i]
        feature_matrix["v(k)"] = self.ma_part[0, max_par:]
        for i in range(1, ma+1):
            feature_matrix[f"v(k-{i})"] = self.ma_part[0, max_par-i:-i]
        
        y = self.time_series[0,max_par:]
        return y, feature_matrix
