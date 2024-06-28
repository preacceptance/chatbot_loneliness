import matplotlib.pyplot as plt
from wordcloud import WordCloud
import pandas as pd
from nltk.stem import SnowballStemmer
import string
from nltk.corpus import stopwords

def plot_word_cloud(file_path="", app="replika"):
    print(app)
    df = pd.read_csv(file_path, sep=",")

    # Convert contains_loneliness_llm to string
    df["contains_loneliness_llm"] = df["contains_loneliness_llm"].apply(lambda x: str(x))

    # Only select the reviews that has contains_loneliness_llm == 1
    df = df[df["contains_loneliness_llm"] == "1"]

    text = " ".join(df["review"].tolist())
    stop_words = set(stopwords.words('english'))
    table = str.maketrans('', '', string.punctuation)

    # Add "app" to stop_words
    stop_words.add("app")
    stop_words.add("App")
    stop_words.add("s")
    stop_words.add("S")
    
    filtered_words = []
    words = text.split()
    ps = SnowballStemmer('english')
    for w in words:
        if w not in stop_words:
            filtered_words.append(ps.stem(w.translate(table).lower()))

    text = " ".join(filtered_words)

    wordcloud = WordCloud(width=2000, height=1000, random_state=1,
                stopwords = None).generate(text)
    plt.figure(figsize=(30, 16))
    plt.imshow(wordcloud) 
    plt.axis("off") 
    plt.tight_layout(pad = 0) 

    # Save in in a pdf file
    plt.savefig("wordcloud_reviews_{}.pdf".format(app))

if __name__ == "__main__":
    for app in ["replika", "chai", "igirl", "simsimi", "cleverbot", "chatgpt"]:
        plot_word_cloud(file_path=f"./e1b_data/review_data/{app}/results/results_with_inference.csv", app=app)