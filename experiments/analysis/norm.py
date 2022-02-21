import matplotlib.pyplot as plt
from statistics import mean, variance
import numpy as np
import scipy.stats as stats
import math


def get_nums(mu, variance):
	sigma = math.sqrt(variance)
	x = np.linspace(mu - 3*sigma, mu + 3*sigma, 100)
	return x, sigma


small_nums = [.01, .015, .03, .044]
large_nums = [.53, .5, .6, .58]


mu1 = mean(small_nums)
var1 = variance(small_nums)
x1, sigma1 = get_nums(mu1, var1)
plt.plot(x1, stats.norm.pdf(x1, mu1, sigma1), label='(isa ?o1 CVSmall)')


mu2 = mean(large_nums)
var2 = variance(large_nums)
x2, sigma2 = get_nums(mu2, var2)
plt.plot(x2, stats.norm.pdf(x2, mu2, sigma2), label='(isa ?o2 CVLarge)')



# mu1 = 0.6
# var1 = .1
# x1, sigma1 = get_nums(mu1, var1)
# plt.plot(x1, stats.norm.pdf(x1, mu1, sigma1), label='(distance ?o1 ?o2 ?quantity)')

# mu2 = 5
# var2 = 6.0
# x2, sigma2 = get_nums(mu2, var2)
# plt.plot(x2, stats.norm.pdf(x2, mu2, sigma2), label='(sizeOf ?o1 ?quantity)')

# mu3 = .5
# var3 = 8.0
# x3, sigma3 = get_nums(mu3, var3)
# plt.plot(x3, stats.norm.pdf(x3, mu3, sigma3), label='(sizeOf ?o2 ?quantity)')


plt.legend()
plt.show()