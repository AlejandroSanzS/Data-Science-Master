#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

@author: pfr
"""

import numpy as np
#import pandas as pd
#pandas
#bb=pd.read_csv('data1.txt', sep='\t',header=None)



# Read text file and suppress last (void) column
data = np.genfromtxt('cdata3.txt', delimiter = '\t')
data = data[:,:-1]  # La ultima fila esta vacia, evitamos su carga
#print(data.shape)

# Read text file and suppress last (void) column
labs = np.genfromtxt('clabs3.txt', delimiter = '\t')
labs = labs[:,:-1]
labs = labs.astype(int)
labs = labs - labs.min() # start labels at zero
#print(labs.shape)

# size, number of features ...
n,d = data.shape

# ... and the number of classes
c=len(np.unique(labs))

print("Dataset of size ",n,", ",d," attributes and ",c," classes\n")







