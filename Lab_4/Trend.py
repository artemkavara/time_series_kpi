import pandas as pd
from sklearn.linear_model import LinearRegression

class Trend(object):
    def __init__(self, trend_rate):
        
        self.trend_rate = trend_rate
        self.coefs = None
        self.trend_model = None
        
    def decompose(self, X):
        df = pd.DataFrame({'time_series':X,'k_1':range(len(X))})
        for i in range(1,self.trend_rate):
            df['k_'+str(i+1)] = df['k_1']**(i+1)
        model = LinearRegression()
        model.fit(df.drop(columns='time_series'), df['time_series'])
        self.coefs = [model.intercept_] + list(model.coef_)
        self.trend_model = lambda x: model.predict(x)
        return model.predict(df.drop(columns='time_series'))
    
    def decompose_extr(self, X):
        df = pd.DataFrame({'k_1':X})
        for i in range(1,self.trend_rate):
            df['k_'+str(i+1)] = df['k_1']**(i+1)
        return self.trend_model(df)