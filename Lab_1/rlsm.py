import numpy as np
import pandas as pd

class RecursiveLeastSquaresMethod():

    def __init__(self, y:np.array, feature_matrix:pd.DataFrame):

        self.y = y
        self.feature_matrix = feature_matrix

    def fit(self, full = 0):
        self.feature_matrix = self.feature_matrix.to_numpy()
        num_obs = self.feature_matrix.shape[0]
        num_feat = self.feature_matrix.shape[1]
        theta = np.zeros((num_feat, 1))
        P = 1000 * np.eye(num_feat)
        theta_history = [] if full else None
        for i in range(num_obs):

            temp_num = P @ self.feature_matrix[i,].reshape(num_feat, 1)
            temp_num = temp_num.reshape(num_feat, 1)
            temp_num = temp_num @ self.feature_matrix[i,].reshape(1, num_feat)
            temp_num = temp_num @ P

            temp_denom = self.feature_matrix[i,].reshape(1, num_feat) @ P
            temp_demom = temp_denom @ self.feature_matrix[i,].reshape(num_feat, 1)
            temp_denom = 1 + temp_demom

            P = P - temp_num/temp_denom

            theta_temp = P @ self.feature_matrix[i,].reshape(num_feat, 1)
            theta_temp = theta_temp*(self.y[i] - 
                self.feature_matrix[i,].reshape(1, num_feat)@theta)
                
            theta = theta + theta_temp
            if full:
                theta_history.append(theta.flatten())
        
        return theta_history if full else theta
