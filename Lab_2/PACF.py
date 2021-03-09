import numpy as np

class PACF:
    
    def __init__(self, time_s):
        self.time_s = np.array(time_s)
        self.mean = self.time_s.mean()
        self.var = self.time_s.var(ddof=1)
        self.result= {}
    
    def r(self, s):
        r = sum((self.time_s[i] - self.mean) * (self.time_s[i-s] - self.mean) for i in range(s, len(self.time_s)))
        return r/((self.var)*(len(self.time_s) - 1))
        
    def F(self, k,j):
        
        if (k,j) in self.result.keys():
            return self.result[(k,j)]
        else:
            num = self.r(k) - sum(self.F(k - 1, i) * self.r(k - i) for i in range(1, k))
            den = 1 - sum(self.F(k - 1, i) * self.r(i) for i in range(1, k))
            f = num/den

        if k == j :
            self.result[(k, j)]= f
            return self.result[(k, j)]
        else:
            self.result[(k, j)] = self.F(k - 1, j) - f * self.F(k - 1, k - j)
            return self.result[(k, j)]
        
    def find_F(self, k):
        return self.F(k,k)