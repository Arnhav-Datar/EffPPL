from math import *
import numpy as np

ls = [1]
cn = 100
alp = 1
bet = 1.05

for i in range(cn):
	nxt = np.random.randn() / 4 
	prv = ls[-1]
	nxt += prv * bet
	nxt += alp
	ls.append(nxt)

print("[", end="")
for i in range(len(ls)):
	print(ls[i], end=" ;")
print("\b]")