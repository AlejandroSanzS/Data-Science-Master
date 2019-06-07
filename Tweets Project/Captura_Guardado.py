# Import modules
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream
from pymongo import MongoClient
import json
import datetime

# Your credentials go here
consumer_key = '8c7VlZNhvdFAPNdd0LC244246'
consumer_secret = 'vtPdScrP3Sdj28yCvJIETFREEsdjIqn3xVk56s2SIU2zpLmFCM'
access_token = '900106233272819712-xa6nJKoNvMuF5UWkCO8dVmcL1ZrlkXh'
access_secret = 'noD7jSMfs05CWR8KfNcDPC7STrT5eJOANy6YmE9IWGlQK'


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
                if status['geo']:
                    created_at = status['created_at']
                    created_at = datetime.datetime.strptime(created_at, '%a %b %d %H:%M:%S +0000 %Y')
                    user_name = status['user']['screen_name']
                    text = status['text']
                    lat = str(status['coordinates']['coordinates'][1])
                    lon = str(status['coordinates']['coordinates'][0])
                    rts = status['retweet_count']
                    favs = status['favorite_count']
                    lang = status['user']['lang']
                    print(status['text'])
                    client = MongoClient('localhost', 27017)
                    db = client['Captura1']
                    collection = db['DatosSabado21']
                    tweet = {'date': created_at, 'user': user_name, 'tweet': text,
                             'latitude': lat, 'longitude': lon, 'language': lang, 'retweets':rts, 'favourites':favs}
                    collection.insert_one(tweet)
        except BaseException as e:
            print("Error on_data: %s" % str(e))


if __name__ == '__main__':
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_secret)

    stream = Stream(auth, l)
    stream.sample()