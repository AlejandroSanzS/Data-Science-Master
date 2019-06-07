from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

import json

from confluent_kafka import Producer



# Your credentials go here
consumer_key = '8c7VlZNhvdFAPNdd0LC244246'
consumer_secret = 'vtPdScrP3Sdj28yCvJIETFREEsdjIqn3xVk56s2SIU2zpLmFCM'
access_token = '900106233272819712-xa6nJKoNvMuF5UWkCO8dVmcL1ZrlkXh'
access_secret = 'noD7jSMfs05CWR8KfNcDPC7STrT5eJOANy6YmE9IWGlQK'


KAFKA_BOOTSTRAP_SERVERS = "localhost:9092,localhost:9093,localhost:9094"
TOPIC1 = "Tweets"
p = Producer({
    'bootstrap.servers': KAFKA_BOOTSTRAP_SERVERS,
    'default.topic.config': {'acks': 'all'}

})


def put_to_stream(data):
    p.poll(0)
    payload = json.dumps(data).encode('utf-8')
    p.produce(TOPIC1, payload)





class StdOutListener(StreamListener):
    """ A listener handles tweets that are received from the stream.
    This is a basic listener that just prints received tweets to stdout.
    """

    def on_error(self, status_code):
        if status_code == 420:
            return False
        else:
            print('ERROOOOOR:' + repr(status_code))
            return True

    def on_data(self, raw_data):
        status = json.loads(raw_data)
        try:

            if 'delete' not in status:  # Tweepy también detecta cuando se ha eliminado un tweet
                if  status['lang']=='en':
                    created_at = str(status['created_at'])
                    user_name = status['user']['screen_name']
                    text = status['text']
                    lang = status['lang']
                    
                    tweet = {'date': created_at, 'user': user_name, 'tweet': text,
                              'language': lang}
                    #print(tweet)
                    print('...')
                    put_to_stream(tweet)
                    
        except BaseException as e:
            print("Error on_data: %s" % str(e))

print('Produciendo tweets capturados:')
if __name__ == '__main__':
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_secret)

    stream = Stream(auth, l)
    stream.filter(track=['#GOT'])