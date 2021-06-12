#! /usr/bin/env python3 


# import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
# import scipy


class DiscreteRandomVariable:
    def __init__(self, a: int = 0, b : int = 1):
        self.variableType = ""
        self.low = a
        self.high = b
        return
    def draw(self, numberOfSamples : int):
        samples = np.random.randint(self.low, self.high, numberOfSamples)
        return samples

DieRolls = DiscreteRandomVariable(1, 6)
plt.hist(DieRolls.draw(10000), bins = [1,2,3,4,5,6,7], align = 'mid')
plt.xlabel('Value')
plt.ylabel('Occurrences')
plt.legend(['Die Rolls'])
plt.savefig("dice.png", dpi=200)