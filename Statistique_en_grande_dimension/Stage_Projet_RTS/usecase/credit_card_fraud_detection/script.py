


sc = SparkContext('local[*]')  # if using locally
sql_sc = SQLContext(sc)


file = "/home/malick/Bureau/Stage_Projet_RTS/usecase/credit_card_fraud_detection/data/creditcard.csv"
#https://www.kaggle.com/harlfoxem/housesalesprediction

pandas_df = pd.read_csv(file)  # assuming the file contains a header

s_df = sql_sc.createDataFrame(pandas_df)

s_df