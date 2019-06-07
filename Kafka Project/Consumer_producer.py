#### Ejemplo Consumidor
# %%
from confluent_kafka import Consumer, Producer
from pymongo import MongoClient
from json import load
import json

from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer


KAFKA_BOOTSTRAP_SERVERS = "localhost:9092,localhost:9093,localhost:9094"
KAFKA_CONSUMER_GROUP = "Grupo"
TOPIC1 = "Tweets"
TOPIC2 = "ClasiNeu"
TOPIC3 = "ClasiNeg"
TOPIC4 = "ClasiPos"



analyser = SentimentIntensityAnalyzer()

def sentiment_analyzer_scores(sentence):
    score = analyser.polarity_scores(sentence)
    return(score['compound'])

c = Consumer({
    'bootstrap.servers': KAFKA_BOOTSTRAP_SERVERS,
    'group.id': KAFKA_CONSUMER_GROUP,
})

p2 = Producer({
    'bootstrap.servers': KAFKA_BOOTSTRAP_SERVERS,
    'default.topic.config': {'acks': 'all'}
})

c.subscribe(['Tweets'])

#client = MongoClient('localhost:27017')
#collection = client.mytopic.mytopic

print('Consumiendo tweets:')

while True:

    msg = c.poll(1.0)
    
    if msg is None:
        continue

    message =json.loads(msg.value().decode("utf-8"))
    #print(message)
    polaridad = sentiment_analyzer_scores(message['tweet'])
    tweet_neg=[]
    tweet_pos=[]
    tweet_neu=[]
    
    if polaridad<0:
        tweet_neg = json.dumps(message).encode("utf-8")
        #print("Tweet negativo:", tweet_neg)
        print('Produciendo tweet negativo')
        p2.produce(TOPIC3, tweet_neg)
    elif polaridad==0:
        
        tweet_neu = json.dumps(message).encode("utf-8")
        print('Produciendo tweet neutro')
        p2.produce(TOPIC2, tweet_neu)
        
    elif polaridad>0:
        tweet_pos = json.dumps(message).encode("utf-8")
        print('Produciendo tweet positivo')
        p2.produce(TOPIC4, tweet_pos)
        
        
c.close()


