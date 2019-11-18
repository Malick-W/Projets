from pyspark import SparkContext
from pyspark.sql import SQLContext
import pandas as pd


sc = SparkContext('local','example')  # if using locally
sql_sc = SQLContext(sc)


file = "/home/malick/Bureau/Stage_Projet_RTS/usecase/house_sales/data/house_data.csv"
#https://www.kaggle.com/harlfoxem/housesalesprediction

house = pd.read_csv(file)  # assuming the file contains a header

s_df = sql_sc.createDataFrame(pandas_df)

