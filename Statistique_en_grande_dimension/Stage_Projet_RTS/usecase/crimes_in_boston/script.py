from pyspark import SparkContext
from pyspark.sql import SQLContext
import pandas as pd
from io import StringIO



sc = SparkContext('local','example')  # if using locally
sql_sc = SQLContext(sc)


file = "/home/malick/Bureau/Stage_Projet_RTS/usecase/crimes_in_boston/data/crime.csv"
#https://www.kaggle.com/harlfoxem/housesalesprediction

boston = pd.read_csv(file, encoding = "ISO-8859-1")  # assuming the file contains a header

s_df = sc.createDataFrame(pandas_df)

s_df

