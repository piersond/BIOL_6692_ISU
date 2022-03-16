import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

### See https://matplotlib.org/stable/tutorials/introductory/pyplot.html
#   for more examples and documentation

# Import data into pandas table from .csv
csv_path = r'C:\github\BIOL_6692_ISU\Lecture 4\RCrk_data.csv'
df = pd.read_csv(csv_path)
print(df)

# Create a scatter plot
plt.scatter('SOC_field', 'pGEP_MSAVI2', s=30, c='pMAST', data=df)
plt.ylabel('Predicted GEP')
plt.xlabel('Field SOC')
plt.legend()
plt.colorbar()
plt.show()

# Create a 3 panel plot
names = ['group_a', 'group_b', 'group_c']
values = [1, 10, 100]

plt.figure(figsize=(9, 3))

plt.subplot(131)
plt.bar(names, values)

plt.subplot(132)
plt.scatter(names, values)

plt.subplot(133)
plt.plot(names, values)

plt.suptitle('Categorical Plotting')
plt.show()