import numpy as np

def moving_average(a, n, weights_name):

    if weights_name == 'exp':
        weights = np.flip((1 - 2/(n+1))**np.arange(1,n+1))
    elif weights_name == 'norm':
        weights = np.ones(n)
    
    ma = []
    for i in range(len(a)-n+1):
        ma.append(a[i:i+n]@weights / sum(weights))
        
    return np.array(ma), weights