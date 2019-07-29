import os, sys, os.path
import twitter
import util

def get_title(filepath):
    with open(filepath) as f:
        return util.parse_md(f)['title']

def get_url(filepath):
    basename = os.path.basename(filepath)
    basename = basename.replace('.md', '.html')
    return f"https://reallyeli.com/posts/{basename}"

if __name__ == "__main__":
    consumer_key = os.environ['TWITTER_API_CONSUMER_KEY']
    consumer_secret = os.environ['TWITTER_API_CONSUMER_SECRET']
    access_token_key = os.environ['TWITTER_API_ACCESS_TOKEN_KEY']
    access_token_secret = os.environ['TWITTER_API_ACCESS_TOKEN_SECRET']

    filepath = sys.argv[1]
    api = twitter.Api(consumer_key=consumer_key, consumer_secret=consumer_secret, access_token_key=access_token_key, access_token_secret=access_token_secret)
    api.PostUpdate(f"New post: {get_title(filepath)} {get_url(filepath)}")
