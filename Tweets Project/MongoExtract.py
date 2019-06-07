from pymongo import MongoClient
import pandas as pd

client = MongoClient('localhost', 27017)
db = client['Captura']
collection = db['test_NOVA']
data = pd.DataFrame(list(collection.find({})))
data.to_csv('test_NOVA.csv')
print(data['date'].apply(lambda t: t.strftime('%d-%m-%Y %H:%M')))
