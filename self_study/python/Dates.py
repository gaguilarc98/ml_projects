# -*- coding: utf-8 -*-
"""
Created on Fri Feb  9 22:22:17 2024

@author: 98acg
"""

import pandas as pd
import numpy as np
import pendulum as pdl

from pandas.tseries.offsets import MonthEnd
from datetime import datetime, timedelta
#%%
b1 = pd.to_datetime("2023-02-01")
b2 = pd.to_datetime("2024-01-01")

d1 = datetime.strptime("2023-02-01", "%Y-%m-%d")
d2 = datetime.strptime("2024-01-01", "%Y-%m-%d")

dif = d2-d1