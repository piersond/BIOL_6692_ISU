import pandas as pd
import numpy as np

### See https://pandas.pydata.org/pandas-docs/stable/user_guide/10min.html
#   for more examples and documentation

### Creating pandas data frames
# Import data into pandas table from .csv
csv_path = r'C:\github\BIOL_6692_ISU\Lecture 4\RCrk_data.csv'
df = pd.read_csv(csv_path)
print(df)

# Manually create a pandas data frame 
df2 = pd.DataFrame(
    {
        "A": 1.0,
        "B": pd.Timestamp("20130102"),
        "C": pd.Series(1, index=list(range(4)), dtype="float32"),
        "D": np.array([3] * 4, dtype="int32"),
        "E": pd.Categorical(["test", "train", "test", "train"]),
        "F": "foo",
    }
)
print(df2)

### Checking column data types (aka structure)
print(df2.dtypes)

### Viewing data
print(df.head(5))
print(df.tail(3))

### Data selection
# By column name
print(df["ID"])

# By row number
print(df[0:3])

# By index
print(df.iloc[3:5, 0:2]) # Goes up to, but does not include, the last index number
print(df.iloc[[3, 4, 5], [0, 1, 2]]) # Use to get values from exact index numbers
