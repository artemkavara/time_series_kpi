import numpy as np

def RSS(time_series_old, time_series_new):

    return (np.linalg.norm(time_series_new - time_series_old)) ** 2

def R_squared(time_series_old, time_series_new):

    TSS = np.linalg.norm(time_series_old - time_series_old.mean())**2
    ESS = np.linalg.norm(time_series_new - time_series_new.mean())**2

    return ESS/TSS

def IKA(time_series_old, time_series_new, ar, ma):

    return time_series_old.size * np.log(RSS(time_series_old, time_series_new)) + 2*(ar+ma+1)

