

#### Ejemplo Productor twitter
# %%
# CUIDADO CON LAS CREDENCIALES EN LAS LIBRETAS! ESTO ES SOLO TEMPORAL!


import json
import threading
from confluent_kafka import Consumer


KAFKA_BOOTSTRAP_SERVERS = "localhost:9092,localhost:9093,localhost:9094"
KAFKA_CONSUMER_GROUP = "Grupo"
TOPIC1 = "Tweets"
TOPIC2 = "ClasiNeu"
TOPIC3 = "ClasiNeg"
TOPIC4 = "ClasiPos"


class ConsumerPos(threading.Thread):
    
    def __init__(self, out):
        super(ConsumerPos, self).__init__()
        
        self.out = out
        
        self.kafka_consumer = Consumer({
            'bootstrap.servers': KAFKA_BOOTSTRAP_SERVERS,
            'group.id': KAFKA_CONSUMER_GROUP
        })
            

        self.kafka_consumer.subscribe([TOPIC4])

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

print("Los tweets positivos consumidos son:")
out_pos = print()
consumer_pos = ConsumerPos(out_pos)
consumer_pos.run()
consumer_pos.stop()