# -*- coding: utf-8 -*-
"""
Created on Sat May 25 21:02:27 2019

@author: Maria
"""

import json
import threading
from confluent_kafka import Consumer

KAFKA_BOOTSTRAP_SERVERS = "localhost:9092,localhost:9093,localhost:9094"
KAFKA_CONSUMER_GROUP = "Grupo"
TOPIC1 = "Tweets"
TOPIC2 = "ClasiNeu"
TOPIC3 = "ClasiNeg"
TOPIC4 = "ClasiPos"

class ConsumerNeg(threading.Thread):
    
    def __init__(self, out):
        super(ConsumerNeg, self).__init__()
        
        self.out = out
        
        self.kafka_consumer = Consumer({
            'bootstrap.servers': KAFKA_BOOTSTRAP_SERVERS,
            'group.id': KAFKA_CONSUMER_GROUP
        })
            

        self.kafka_consumer.subscribe([TOPIC3])

        self.active = True
        
        
    def run(self):
        while self.active:

            tweet = self.kafka_consumer.poll(1.0)

            if tweet is None:
                continue

            message = tweet.value().decode('utf-8')
            message = json.loads(message)
            print(message)
            #self.account_db[data["account"]] += data["amount"]        
            #self.out.clear_output()
            #self.out.append_display_data(self.account_db)

        self.kafka_consumer.close()
        
        
    def stop(self):
        self.active = False

print("Los tweets negativos consumidos son:")
out_neg = print()

consumer_neg = ConsumerNeg(out_neg)
consumer_neg.run()

consumer_neg.stop()