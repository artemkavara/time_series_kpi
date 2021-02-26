import numpy as np
import pandas as pd

class LeastSquaresMethod():

    def __init__(self, y:np.array, feature_matrix:pd.DataFrame):

        self.y = y
        self.feature_matrix = feature_matrix

    def fit(self):

        self.feature_matrix=self.feature_matrix.to_numpy()
        inform_matr = np.linalg.inv(self.feature_matrix.T@self.feature_matrix)
        inform_matr = inform_matr @ self.feature_matrix.T
        return inform_matr@self.y