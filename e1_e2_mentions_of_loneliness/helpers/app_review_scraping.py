import pandas as pd
from app_store_scraper import AppStore

def get_reviews(app_id, app_name, sleep):
    app = AppStore(country='us', app_name=app_name, app_id = app_id)
    app.review(sleep=sleep)
    app_df = pd.DataFrame(app.reviews)
    app_df.to_csv(f'{app_name}_reviews.csv', index=False)

# Main function, that will take in the parameters of get_reviews from the console
if __name__ == "__main__":
    import sys
    
    app_name = sys.argv[1]
    app_id = sys.argv[2]
    sleep = 2 #int(sys.argv[3])
    get_reviews(app_id, app_name, sleep)

    # Three most popular apps after searching 'ai companion' in app store with more than 10K ratings:
    # Replika: app_name='replika-virtual-ai-companion', app_id = '1158555867' 217.1K
    # Chai: app_name='chai-chat-with-ai-bots', app_id = '1544750895' 100.1K
    # iGirl: app_name='igirl-sexy-ai-girlfriend-game', app_id = '1560554635' 18K


    # SimSimi: app_name='simsimi', app_id = '375239755' 7.3K
    # Cleverbot: app_name='cleverbot', app_id = '383010873' 479

    
    # Eva AI: app_name='eva-ai-chat-clever-chatbot', app_id = '1551794721' 20.6K Ratings

    # ChatGPT: app_name='chatgpt', app_id = '6448311069'
    # pi: app_name='pi-personal-ai-assistant', app_id = '6445815935