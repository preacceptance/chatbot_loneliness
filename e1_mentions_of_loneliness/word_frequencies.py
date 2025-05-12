import pandas as pd
from nltk.stem import SnowballStemmer
import string
from nltk.corpus import stopwords
from collections import Counter
import pandas as pd

def print_word_frequency_table(app_list):
    rows = []
    for app in app_list:
        file_path = f"./review_data/review_data/{app}/results/results_with_inference.csv"
        df = pd.read_csv(file_path, sep=",")
        df["contains_loneliness_llm"] = df["contains_loneliness_llm"].astype(str)
        df = df[df["contains_loneliness_llm"] == "1"]
        text = " ".join(df["review"].tolist())

        stop_words = set(word.lower() for word in stopwords.words('english'))
        stop_words.update(["app", "s", "it", "its", "it's", "It's", ""])

        table = str.maketrans('', '', string.punctuation)
        ps = SnowballStemmer('english')

        cleaned_words = [
            w.translate(table).lower()
            for w in text.split()
        ]

        cleaned_words = [
            w for w in cleaned_words
            if w not in stop_words
        ]

        filtered_words = [
            ps.stem(w) for w in cleaned_words
        ]

        # Remove "it" from filtered_words
        filtered_words = [w for w in filtered_words if w != "it"]

        word_counts = Counter(filtered_words)
        
        total_words = len(filtered_words)
        top_words = word_counts.most_common(5)

        row = {"App": app.capitalize()}
        for i, (word, count) in enumerate(top_words, 1):
            pct = count / total_words * 100 if total_words else 0
            row[f"Word {i}"] = f"{word} {pct:.1f}% ({count}/{total_words})"
        rows.append(row)

    table_df = pd.DataFrame(rows)
    print("\nTop 5 Most Frequent Words in Loneliness-Related Reviews:")
    print(table_df.to_string(index=False))
    table_df.to_csv("top5_words_by_app.csv", index=False)


if __name__ == "__main__":
    all_results = []
    app_list = ["wysa", "replika", "chai", "igirl", "simsimi", "cleverbot", "chatgpt"]

    print_word_frequency_table(app_list)