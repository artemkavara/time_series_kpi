import numpy as np

def generate_ma_part(length = 100):
    return np.random.normal(size = (length,1))

class TimeSeries():

    def __init__(self, a_coef:np.array, b_coef:np.array, ma_part = None, length = 100):

        self.ar_coef = a_coef
        self.ma_coef = b_coef
        self.length = length
        self.ma_part = generate_ma_part(length) if ma_part is None else ma_part.T
        self.ar = len(a_coef)-1
        self.ma = len(b_coef)-1

    def time_series_generate(self):

        self.time_series = np.zeros((self.length,1))
        self.time_series[0:self.ar] = self.ar_coef[0]+self.ma_coef[0]*self.ma_part[0:self.ar].copy()

        for i in range(self.ar, self.length):
            self.time_series[i] = self.ar_coef[0]+(
                            sum([self.ar_coef[j]*self.time_series[i-j] for j in range(1, self.ar+1)])+
                            self.ma_coef[0]*self.ma_part[i]+
                            sum([self.ma_coef[j]*self.ma_part[i-j] for j in range(1, self.ma+1)]))
    
    def save_time_series(self, file):
        np.savetxt(file, self.time_series, fmt = "%.5f")
    
    def save_ma_part(self, file):
        np.savetxt(file, self.ma_part, fmt = "%.5f")