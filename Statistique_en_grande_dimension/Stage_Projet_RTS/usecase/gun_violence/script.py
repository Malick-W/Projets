data = spark.read.format("csv").option("header", "true") \
							   .option("delimiter", ",") \
							   .option("inferSchema", "true") \
				 .load(file)



file = "/home/malick/Bureau/Stage_Projet_RTS/usecase/gun_violence/data/gun-violence-data_01-2013_03-2018.csv"


data = pd.read_csv(file)