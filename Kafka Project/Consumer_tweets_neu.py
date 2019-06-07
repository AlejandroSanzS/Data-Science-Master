# -*- coding: utf-8 -*-
"""
Created on Sat May 25 21:04:58 2019

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


class ConsumerNeu(threading.Thread):
    
    def __init__(self, out):
        super(ConsumerNeu, self).__init__()
        
        self.out = out
        
        self.kafka_consumer = Consumer({
            'bootstrap.servers': KAFKA_BOOTSTRAP_SERVERS,
            'group.id': KAFKA_CONSUMER_GROUP
        })
            

        self.kafka_consumer.subscribe([TOPIC2])

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

print("Los tweets neutros consumidos son:")
out_neu = print()
consumer_neu = ConsumerNeu(out_neu)
consumer_neu.run()

consumer_neu.stop()