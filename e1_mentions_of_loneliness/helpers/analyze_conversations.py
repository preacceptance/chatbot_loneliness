import os
import random
import string
import json
import re
from pprint import pprint
import copy
import numpy as np
from nltk.stem import SnowballStemmer
import csv
from datetime import datetime
import pandas as pd
from transformers import pipeline
import pandas as pd
import pingouin

model_path = "cardiffnlp/twitter-roberta-base-sentiment-latest"
classifier_sentiment = pipeline(
    "sentiment-analysis", model=model_path, tokenizer=model_path, truncation=True, max_length=512)


def contains_word(sentence, word):
    if type(word) == list:
        for w in word:
            if (' ' + w + ' ') in (' ' + sentence + ' '):
                return True
        return False
    else:
        return (' ' + word + ' ') in (' ' + sentence + ' ')


def sort_by_datetime(content):
    dts = copy.deepcopy(content['datetime'])
    dts.sort()
    sorted_content = {}
    for i, d in enumerate(dts):
        false_index = content['datetime'].index(d)
        for k in content.keys():
            if k == 'id':
                if k not in sorted_content.keys():
                    sorted_content[k] = content[k]
                continue
            if k not in sorted_content.keys():
                sorted_content[k] = []
            sorted_content[k].append(content[k][false_index])

    return sorted_content


# For Reading data:
def read_data():
    contents = {'datetime': [], 'index': [], 'language': [],
                'chatbot_messages': [], 'human_messages': [], 'both': [], 'id': ''}
    data_combined = []

    def add_content(fp, content):
        with open(fp) as infile:
            last_user = None
            for line in infile:
                line_content = line.split('\t')

                if last_user is None:
                    last_user = line_content[2]
                    content['id'] = last_user
                elif line_content[2] != last_user:
                    # Take only if the language is English. Had to do this bc. some messages are labeled incorrectly
                    if content['language'].count('en') / len(content['language']) > 0.9:
                        # Sort by message date before adding
                        content = sort_by_datetime(content)
                        data_combined.append(content)
                    content = {'datetime': [], 'index': [], 'language': [], 'chatbot_messages': [],
                               'human_messages': [], 'both': [],
                               'id': line_content[2]}
                    last_user = line_content[2]  # Update user id

                content['datetime'].append(str_to_datetime(line_content[0]))
                content['index'].append(line_content[3])
                content['language'].append(line_content[4])
                content['chatbot_messages'].append(line_content[5])
                content['human_messages'].append(line_content[6].strip('\n'))
                content['both'].append(line_content[5])
                content['both'].append(line_content[6].strip('\n'))

    with open("./conversation_data/cleverbot/ids_USCan.txt", "r") as ifile:
        uscan_ids = ifile.readlines()
    uscan_ids = [id.strip() for id in uscan_ids]

    add_content('./conversation_data/cleverbot/20220202/20220202.txt', contents)
    add_content('./conversation_data/cleverbot/20210913/20210913.txt', contents)

    fname = "./conversation_data/cleverbot/raw_messages.json"

    # Only include the conversations whose id is in uscan_ids
    data_combined = [
        content for content in data_combined if content['id'] in uscan_ids]

    # Print how many unique conversations and users there are
    print("Number of conversations: ", len(data_combined))
    print("Number of users: ", len(
        set([content['id'] for content in data_combined])))

    with open(fname, 'w+') as json_file:
        json.dump(data_combined, json_file, default=str)


# For Reading data:
def read_new_dataset_ids():
    contents = {'datetime': [], 'index': [], 'language': [], 'chatbot_messages': [], 'human_messages': [], 'both': [],
                'id': ''}
    data_combined = []

    def add_content(fp, content):
        with open(fp) as infile:
            last_user = None
            for line in infile:
                line_content = line.split('\t')

                if last_user is None:
                    last_user = line_content[2]
                    content['id'] = last_user
                elif line_content[2] != last_user:
                    # Take only if the language is English. Had to do this bc. some messages are labeled incorrectly
                    if content['language'].count('en') / len(content['language']) > 0.9:
                        # Sort by message date before adding
                        content = sort_by_datetime(content)
                        data_combined.append(content)
                    content = {'datetime': [], 'index': [], 'language': [], 'chatbot_messages': [],
                               'human_messages': [], 'both': [],
                               'id': line_content[2]}
                    last_user = line_content[2]  # Update user id

                content['datetime'].append(str_to_datetime(line_content[0]))
                content['index'].append(line_content[3])
                content['language'].append(line_content[4])
                content['chatbot_messages'].append(line_content[5])
                content['human_messages'].append(line_content[6].strip('\n'))
                content['both'].append(line_content[5])
                content['both'].append(line_content[6].strip('\n'))

    # Add both dates
    add_content('./conversation_data/cleverbot/20220202/20220202.txt', contents)
    print("Number of users: ", len(
        set([content['id'] for content in data_combined])))

    add_content('./conversation_data/cleverbot/20210913/20210913.txt', contents)
    print("Number of users: ", len(
        set([content['id'] for content in data_combined])))

    fname = "./conversation_data/cleverbot/raw_messages.json"
    with open(fname, 'w+') as json_file:
        json.dump(data_combined, json_file, default=str)


def str_to_datetime(s, std=False):
    format_str = '%d/%b/%Y:%H:%M:%S %z'
    if std: return datetime.strptime(s, '%Y-%m-%d %H:%M:%S')

    if '\ufeff' in s:
        s = s.replace('\ufeff', '')

    if len(s) == 20:
        format_str = '%d/%b/%Y:%H:%M:%S'

    return datetime.strptime(s, format_str).replace(tzinfo=None)


def get_conversation_duration(conversation):
    dt = conversation['datetime']
    start = str_to_datetime(dt[0], std=True)
    end = str_to_datetime(dt[-1], std=True)
    duration_in_s = (end - start).total_seconds()
    duration_in_mins = round((duration_in_s / 60), 2)

    return str(start), start.hour, duration_in_mins


def get_time_stats(app='cleverbot'):
    with open('./conversation_data/{}/formatted_messages.json'.format(app)) as json_file:
        data = json.load(json_file)

    total_duration = 0
    day = 0  # Conversation Amount During the Day
    night = 0  # Conversation Amount During the Night (22pm - 6am)
    all_hours = [0] * 24
    for d in data['human']:
        total_duration += d['duration_in_mins']
        start_hour = str_to_datetime(d['start_time'], std=True).hour
        if start_hour >= 22 or start_hour <= 6:
            night += 1
        else:
            day += 1

        all_hours[start_hour - 1] += 1

    avg_duration = total_duration / len(data['human'])
    stats = {}
    stats['avg_duration'] = avg_duration
    stats['day_count'] = day
    stats['day_night'] = night
    stats['all_hrs'] = [{(i + 1): h} for i, h in enumerate(all_hours)]
    stats['conversation_amt'] = len(data['human'])

    pprint(stats)


# Divides conversations into smaller ones if the time difference between messages is more than 30 minutes
def divide_conversations(chatlog):
    new_chatlog = []
    for i in range(0, len(chatlog)):
        conversation = chatlog[i]
        start_indexes = [0]
        for j in range(0, len(conversation['datetime']) - 1):
            curr_datetime = str_to_datetime(
                conversation['datetime'][j], std=True)
            next_datetime = str_to_datetime(
                conversation['datetime'][j + 1], std=True)
            diff_sec = (next_datetime - curr_datetime).total_seconds()
            if (diff_sec / 60) > 30 or diff_sec < 0:
                start_indexes.append(j + 1)

        start_indexes.append(len(conversation['datetime']))
        if len(start_indexes) > 2:
            for k in range(0, len(start_indexes) - 1):
                splitted_msg = {'datetime': conversation['datetime'][start_indexes[k]: start_indexes[k + 1]],
                                'index': conversation['index'][start_indexes[k]: start_indexes[k + 1]],
                                'language': conversation['language'][start_indexes[k]: start_indexes[k + 1]],
                                'chatbot_messages': conversation['chatbot_messages'][
                    start_indexes[k]: start_indexes[k + 1]],
                    'human_messages': conversation['human_messages'][
                    start_indexes[k]: start_indexes[k + 1]],
                    'id': conversation['id']}
                new_chatlog.append(splitted_msg)
        else:
            new_chatlog.append(conversation)

    return new_chatlog


def format_messages(app='cleverbot'):
    with open('./conversation_data/{}/raw_messages.json'.format(app)) as json_file:
        data = json.load(json_file)

    # Divide each conversation based on interval. If the interval between two messages is bigger than 30,
    # we have 2 different conversations (or possibly more)
    print("Number of conversations before dividing: ", len(data))
    new_chatlog = divide_conversations(data)
    print("Number of conversations after dividing: ", len(new_chatlog))

    human_messages = []
    chatbot_messages = []

    combined_messages = []
    empty_conv = []

    print_progress_bar(0, len(new_chatlog), prefix='Progress:',
                       suffix='Complete', length=50)
    # Append messages in each conversation together
    for i, conversation in enumerate(new_chatlog):
        ct = get_conversation_duration(conversation)
        human_word_amt = len(
            (' '.join(conversation['human_messages'])).split(' '))
        chatbot_word_amt = len(
            (' '.join(conversation['chatbot_messages'])).split(' '))
        turn_amt = len(conversation['human_messages'])

        if human_word_amt > 0:
            human_messages.append({'conversation': conversation['human_messages'], 'start_time': ct[0],
                                   'duration_in_mins': ct[2], 'human_word_amt': human_word_amt, 'turns': turn_amt,
                                   'chatbot_word_amt': chatbot_word_amt, 'id': conversation['id']})

            chatbot_messages.append({'conversation': conversation['chatbot_messages'],
                                     'start_time': ct[0], 'duration_in_mins': ct[2], 'human_word_amt': human_word_amt,
                                     'turns': turn_amt, 'chatbot_word_amt': chatbot_word_amt, 'id': conversation['id']})

            all_messages = ''
            for k in range(len(conversation['human_messages'])):
                all_messages += 'Chatbot: \'' + conversation['chatbot_messages'][k] + '\' \n Human: \'' + \
                                conversation['human_messages'][k] + '\' \n '

            combined_messages.append(
                {'conversation': all_messages, 'start_time': ct[0], 'duration_in_mins': ct[2],
                 'human_word_amt': human_word_amt, 'chatbot_word_amt': chatbot_word_amt, 'turns': turn_amt,
                 'time_of_all_messages': conversation['datetime'],
                 'id': conversation['id'], 'index': i})
        else:
            empty_conv.append({'conversation': conversation['human_messages'], 'start_time': ct[0],
                               'duration_in_mins': ct[2], 'human_word_amt': human_word_amt, 'turns': turn_amt,
                               'chatbot_word_amt': chatbot_word_amt})

        print_progress_bar(i + 1, len(new_chatlog),
                           prefix='Progress:', suffix='Complete', length=50)

    fname = "./conversation_data/{}/formatted_messages.json".format(app)
    with open(fname, 'w+') as json_file:
        json.dump({'human': human_messages, 'chatbot': chatbot_messages},
                  json_file, default=str, indent=4)

    fname = "./conversation_data/{}/human_readable_messages.json".format(app)
    with open(fname, 'w+') as json_file:
        json.dump(combined_messages, json_file, default=str, indent=4)


# Process Reddit posts
def process_reddit_posts(filename):
    df = pd.read_csv('./reddit_data/{}.csv'.format(filename))

    # Divide each conversation based on interval. If the interval between two messages is bigger than 30,
    # we have 2 different conversations (or possibly more)
    print("Number of posts: ", df.shape[0])
    print_progress_bar(
        0, df.shape[0], prefix='Progress:', suffix='Complete', length=50)

    ps = SnowballStemmer('english')
    table = str.maketrans('', '', string.punctuation)
    print_progress_bar(
        0, df.shape[0], prefix='Progress:', suffix='Complete', length=50)

    # Iterate through each post and process the text. Save this into a new column named 'processed_text'
    for i in range(df.shape[0]):
        words = []
        for m_word in str(df.iloc[i]['titlencontent']).split(' '):
            # Remove punctuation, lower, stem
            words.append(ps.stem(m_word.translate(table).lower()))

        # Save this into a new column named 'processed_text'
        df.at[i, 'processed_text'] = " ".join(words)

        print_progress_bar(
            i + 1, df.shape[0], prefix='Progress:', suffix='Complete', length=50)

    # save the new dataframe into a new csv file
    df.to_csv('./reddit_data/{}_processed.csv'.format(filename), index=False)

# Now find mental health words in the processed text


def find_mental_health_words(filename):
    df = pd.read_csv('./reddit_data/{}.csv'.format(filename))
    loneliness_list = clean_words(
        list(csv.reader(open('../loneliness_dict_additions.csv'))))

    print_progress_bar(
        0, df.shape[0], prefix='Progress:', suffix='Complete', length=50)

    # Find loneliness words in the processed text
    for i in range(df.shape[0]):
        # Get current row
        isec = get_intersections(loneliness_list, df.iloc[i]['titlencontent'], str(
            df.iloc[i]['processed_text']).split(" "))
        if len(isec) > 0:
            df.at[i, 'contains_loneliness_dict'] = True
            df.at[i, 'detected_loneliness_msgs'] = json.dumps(isec)
        else:
            df.at[i, 'contains_loneliness_dict'] = False
            df.at[i, 'detected_loneliness_msgs'] = ""

        print_progress_bar(
            i + 1, df.shape[0], prefix='Progress:', suffix='Complete', length=50)

    # Print percentage of posts that contain loneliness words
    print("Percentage of posts that contains loneliness words: {:.2f}%".format(
        df['contains_loneliness_dict'].sum() * 100 / df.shape[0]))

    # sort by created_date and look at loneliness percentage in the last 30 days
    df['created_date'] = pd.to_datetime(df['created_date'])
    df = df.sort_values(by='created_date', ascending=False)

    # Plot teh number of posts that contain loneliness words over all days
    df['contains_loneliness_dict'] = df['contains_loneliness_dict'].astype(int)
    df['created_date'] = df['created_date'].dt.date

    # Create a new dataframe that contains the number of posts that contain loneliness words over all days
    df2 = df.groupby('created_date').agg(
        {'contains_loneliness_dict': 'sum'}).reset_index()
    df2['total_posts'] = df.groupby('created_date').size(
    ).reset_index(name='total_posts')['total_posts']
    df2['loneliness_percentage'] = df2['contains_loneliness_dict'] / \
        df2['total_posts'] * 100

    # Filter between 2020-06-01 and 2021-06-31 dates (not strings)
    df2['created_date'] = pd.to_datetime(df2['created_date'])
    df2 = df2[(df2['created_date'] >= pd.to_datetime('2020-06-01'))
              & (df2['created_date'] <= pd.to_datetime('2021-06-30'))]

    # Plot number of loneliness and total posts over all days using matplotlib
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots()
    ax.plot(df2['created_date'], df2['contains_loneliness_dict'],
            label='Total Loneliness Posts')
    ax.plot(df2['created_date'], df2['total_posts'], label='Total Posts')
    ax.set_xlabel('Date')
    ax.set_ylabel('Number of Posts')
    ax.set_title('Number of Posts that Contain Loneliness Words Over Time')
    ax.legend()

    # Limit y-axis to 0-50
    ax.set_ylim(0, 50)

    plt.show()

    # Save the new dataframe into a new csv file
    df.to_csv('./reddit_data/{}_final.csv'.format(filename), index=False)


# Remove stopwords & remove punctuation & normalize case
def process_conversations(app='cleverbot'):
    with open('./conversation_data/{}/formatted_messages.json'.format(app)) as json_file:
        data = json.load(json_file)
    ps = SnowballStemmer('english')
    table = str.maketrans('', '', string.punctuation)

    all_convs = {'human': [], 'chatbot': []}
    print_progress_bar(
        0, len(data['human']), prefix='Progress:', suffix='Complete', length=50)
    for i in range(len(data['human'])):  # Each conversation
        for agent_type in ['human', 'chatbot']:
            processed_messages = []
            for message in data[agent_type][i]['conversation']:  # Each message
                words = []
                for m_word in message.split(' '):
                    # Remove punctuation, lower, stem
                    words.append(ps.stem(m_word.translate(table).lower()))
                processed_messages.append(words)
            all_convs[agent_type].append(processed_messages)

        print_progress_bar(
            i + 1, len(data['human']), prefix='Progress:', suffix='Complete', length=50)

    fname = "./conversation_data/{}/processed_messages.json".format(app)
    with open(fname, 'w+') as json_file:
        json.dump(all_convs, json_file, default=str)


def process_reviews(app='replika'):
    reviews = pd.read_csv("./review_data/{}/{}_reviews.csv".format(app, app))
    ps = SnowballStemmer('english')
    table = str.maketrans('', '', string.punctuation)

    processed_messages = []
    for i in range(reviews.shape[0]):
        # Get current row
        review = str(reviews.iloc[i]['title']) + \
            " " + str(reviews.iloc[i]['review'])

        words = []
        for m_word in review.split(' '):
            # Remove punctuation, lower, stem
            words.append(ps.stem(m_word.translate(table).lower()))

        processed_messages.append(words)

    fname = "./review_data/{}/processed_reviews.json".format(app)
    with open(fname, 'w+') as json_file:
        json.dump(processed_messages, json_file, default=str)


def read_json(fname):
    with open(fname, 'r') as json_file:
        data = json.load(json_file)

    return data

# Save human messages in jsonl format, with one message each line. Each message will be in the following format: {'message_index': 0, 'text': 'Hello', 'conversation_id': '1234'}


def save_human_messages(app='cleverbot'):
    with open('./conversation_data/{}/formatted_messages.json'.format(app)) as json_file:
        data = json.load(json_file)

    human_messages = []
    print_progress_bar(
        0, len(data['human']), prefix='Progress:', suffix='Complete', length=50)
    for i in range(len(data['human'])):  # Each conversation
        for j in range(len(data['human'][i]['conversation'])):  # Each message
            human_messages.append(
                {'message_index': j, 'text': data['human'][i]['conversation'][j], 'conversation_id': data['human'][i]['id']})
        print_progress_bar(
            i + 1, len(data['human']), prefix='Progress:', suffix='Complete', length=50)

    fname = "./conversation_data/{}/human_messages.jsonl".format(app)
    with open(fname, 'w+') as json_file:
        for message in human_messages:
            json.dump(message, json_file)
            json_file.write('\n')

# Read loneliness_dict.csv and remove all duplicates


def remove_duplicates():
    loneliness_list = clean_words(
        list(csv.reader(open('../loneliness_dict.csv'))))
    loneliness_list = list(dict.fromkeys(loneliness_list))
    with open("./loneliness_dictionary.csv", "w+") as write_file:
        write_file.write("\n".join(loneliness_list))


def clean_words(word_list, message_only=False):
    ps = SnowballStemmer('english')

    dict_words = []
    if message_only:
        # Remove punctuation & normalize case
        table = str.maketrans('', '', string.punctuation)
        word_list = word_list.translate(table)
        word = word_list.split()
        if len(word) > 1:
            phrase = []
            for w in word:  # Lower case and stem the words
                phrase.append(ps.stem(w.lower()))
            dict_words.append(" ".join(phrase))
        else:
            try:
                dict_words.append(ps.stem(word[0].lower()))
            except:
                return [""]
        return dict_words
    else:
        for word in word_list:
            word = word[0].split()
            if len(word) > 1:
                phrase = []
                for w in word:  # Lower case and stem the words
                    phrase.append(ps.stem(w.lower()))
                dict_words.append(" ".join(phrase))
            else:
                dict_words.append(ps.stem(word[0].lower()))

    cleaned_dict_words = list(dict.fromkeys(dict_words))
    return cleaned_dict_words

def write_stats(app='cleverbot', is_review=False, write_convs=True):
    if is_review:
        write_convs = False

    if not os.path.exists("./conversation_data/{}/results/".format(app)):
        os.makedirs("./conversation_data/{}/results/".format(app))

    loneliness_list = clean_words(list(csv.reader(open('./loneliness_dictionary.csv'))))

    # Save the loneliness words in a txt file with one word per line
    with open("./conversation_data/{}/results/loneliness_words.txt".format(app), "w+") as write_file:
        write_file.write("\n".join(loneliness_list))

    results = get_stats(loneliness_list, app, is_review, write_convs)

    # Write results to a json file
    filename = './conversation_data/{}/results/stats_{}.json'.format(
        app, 'review' if is_review else 'conversation')
    with open(filename, 'w+') as outfile:
        json.dump(results, outfile, indent=4)


def increment_stats(stats, i, data, wtype, start_hour, isections):
    isections[wtype].append(True)
    stats[start_hour]['durations'][wtype].append(data[i]['duration_in_mins'])
    stats[start_hour]['word_counts']['human'][wtype].append(data[i]['human_word_amt'])
    stats[start_hour]['word_counts']['chatbot'][wtype].append(data[i]['chatbot_word_amt'])
    stats[start_hour]['turns'][wtype].append(data[i]['turns'])


def write_results(stats, percentages, avg_durations, avg_length, avg_turns, write_file, isections):
    for skey in stats[0]['durations'].keys():
        percentages['intersect_percentages'][skey] = isections[skey].count(
            True) * 100 / len(isections[skey])

        print("Frequency of conversations that contains {} words: {:.2f}%".format(skey,
                                                                                  percentages['intersect_percentages'][
                                                                                      skey]),
              file=write_file)

        avg_durations[skey] = sum([sum(stat['durations'][skey])
                                  for stat in stats]) / isections[skey].count(True)

        avg_length[skey] = sum([sum(stat['word_counts']['human'][skey]) for stat in stats]) / isections[skey].count(
            True)

        avg_turns[skey] = sum([sum(stat['turns'][skey]) for stat in stats]) / isections[skey].count(
            True)

        print("Average duration of conversations that includes any of the {} words: {:.2f} mins".format(skey,
                                                                                                        avg_durations[
                                                                                                            skey]),
              file=write_file)

        print("Average length (word count) of conversations that includes any of the {} words: {:.2f}".format(skey,
                                                                                                              avg_length[
                                                                                                                  skey]),
              file=write_file)

        print("Average amount of turns of conversations that includes any of the {} words: {:.2f}".format(skey,
                                                                                                          avg_turns[
                                                                                                              skey]),
              file=write_file)

# Returns loneliness words in the conversation


def get_intersections(loneliness, conversation, conversation_cleaned):
    if type(conversation) == str:
        conversation = [conversation]
        conversation_cleaned = [conversation_cleaned]

    loneliness_isec = []
    for m in loneliness:
        for i, message in enumerate(conversation_cleaned):
            if contains_word(" ".join(message), m):
                loneliness_isec.append(
                    {'message_index': i, 'term_found': m, 'message': conversation[i]})
    # If message contains alon or lone but not any other words in loneliness array, then print that message
    # for i, message in enumerate(conversation_cleaned):
    #    if contains_word(" ".join(message), ['alon', 'loneli', 'lone']) and \
    #        not contains_word(" ".join(message), loneliness):
    #        print(" ".join(message))

    return loneliness_isec


def get_random_string(length):
    result_str = ''.join(random.choice(string.ascii_letters)
                         for i in range(length))
    return result_str


def search_conv(convs, id, start_time):
    return [conv['conversation'] for conv in convs if conv['id'] == id and conv['start_time'] == start_time]


# Print iterations progress
def print_progress_bar(iteration, total, prefix='', suffix='', decimals=1, length=100, fill='█', printEnd="\r"):
    """
    Call in a loop to create terminal progress bar
    @params:
        iteration   - Required  : current iteration (Int)
        total       - Required  : total iterations (Int)
        prefix      - Optional  : prefix string (Str)
        suffix      - Optional  : suffix string (Str)
        decimals    - Optional  : positive number of decimals in percent complete (Int)
        length      - Optional  : character length of bar (Int)
        fill        - Optional  : bar fill character (Str)
        printEnd    - Optional  : end character (e.g. "\r", "\r\n") (Str)
    """
    percent = ("{0:." + str(decimals) + "f}").format(100 *
                                                     (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print(f'\r{prefix} |{bar}| {percent}% {suffix}', end=printEnd)
    # Print New Line on Complete
    if iteration == total:
        print()


def classifier_sent(x):
    x = str(x) if x is not None else ""
    res = classifier_sentiment(x)[0]
    return [res['label'], res['score']]


# Write statistics and sentiment analysis results
def get_stats(loneliness_list, app='cleverbot', is_review=False, write_convs=True):
    print("Analysing conversations...")

    # ------------------------- Initialize variables ------------------------- #
    dict_format = {"non_loneliness": [], "loneliness": []}
    stats_hourly = [{"durations": copy.deepcopy(dict_format),
                     "turns": copy.deepcopy(dict_format),
                     "word_counts": {"chatbot": copy.deepcopy(dict_format),
                                     "human": copy.deepcopy(dict_format)}} for i in range(24)]

    avg_stats_hourly = copy.deepcopy(stats_hourly)
    isections = {"non_loneliness": [], "loneliness": []}

    conversation_dict = {}
    date_str = 'start_time' if not is_review else 'date'

    # ------------------------- Read data ------------------------- #
    if not is_review:
        data = read_json("./conversation_data/{}/formatted_messages.json".format(app))['human']
        data_processed = read_json("./conversation_data/{}/processed_messages.json".format(app))['human']

        human_readable = read_json("./conversation_data/{}/human_readable_messages.json".format(app))
        data_chatbot = read_json("./conversation_data/{}/formatted_messages.json".format(app))['chatbot']

        data_df = pd.DataFrame(data)
        data_df["id"] = data_df['id'].astype(str) + "_" + data_df["start_time"]

    if is_review:
        # Read replika_reviews.csv
        data_df = pd.read_csv("./e1b_data/review_data/{}/{}_reviews.csv".format(app, app))

        # Make id row number
        data_df["id"] = data_df.index

        data_processed = read_json("./e1b_data/review_data/{}/processed_reviews.json".format(app))

    # Number of conversations
    print("Number of conversations: {}".format(len(data_df['id'].unique())))

    data_df['contains_loneliness_dict'] = False; data_df['full_conversation'] = ""; data_df['loneliness_message_pairs'] = ""

    if is_review:
        conversations = [str(data_df.iloc[i]['title']) + " " +
                         str(data_df.iloc[i]['review']) for i in range(data_df.shape[0])]
    else:
        conversations = [data[i]['conversation'] for i, r in enumerate(data)]
        conversations_chatbot = [data_chatbot[i]['conversation'] for i, r in enumerate(data_chatbot)]
        conversations_full = [human_readable[i]['conversation'] for i in range(len(human_readable))]

    # Add the LLM results to the DF
    if not is_review:
        df_v5 = pd.read_csv("./e1a_data/manual_classification_v5.csv", sep=";")
        data_df["contains_loneliness_llm"] = data_df["id"].apply(lambda x: 1 if x in df_v5[(df_v5["rater_1"] == 1) & (df_v5["rater_2"] == 1)]["id"].tolist() else 0)

    # ------------------------- Start collecting the data ------------------------- #

    print_progress_bar(0, len(conversations), prefix='Progress:', suffix='Complete', length=50)
    for i, conversation in enumerate(conversations):
        print_progress_bar(i + 1, len(conversations), prefix='Progress:', suffix='Complete', length=50)

        if not is_review: data_df.loc[i, 'full_conversation'] = conversations_full[i]

        if is_review: conv_id = i
        else: conv_id = data[i]['id'] + '_' + str(data[i][date_str])

        # Find loneliness words in conversation:
        loneliness_isec = get_intersections(loneliness=loneliness_list, conversation=conversation,
                                            conversation_cleaned=data_processed[i])

        # If review, then classify sentiment
        if is_review: data_df.loc[i, 'sentiment'] = json.dumps(classifier_sent(conversation))
        start_hour = str_to_datetime(data_df.iloc[i][date_str], std=True).hour

        if len(loneliness_isec) > 0: # Conversation contains loneliness words
            data_df.loc[data_df['id'] == conv_id, 'contains_loneliness_dict'] = True

            loneliness_pairs = []
            last_index = -1

            if not is_review:
                for m in loneliness_isec:
                    if last_index != m['message_index']:
                        loneliness_pairs.append(
                            "Chatbot: " +
                            conversations_chatbot[i][m['message_index']] +
                            "; Human: " + conversation[m['message_index']]
                        )

                    last_index = m['message_index']

                data_df.loc[i, 'loneliness_message_pairs'] = json.dumps(loneliness_pairs)

        if not is_review:
            # Increment stats based on intersection
            if data_df.iloc[i]['contains_loneliness_llm']:
                increment_stats(stats_hourly, i, data, 'loneliness', start_hour, isections)
                isections['non_loneliness'].append(False)
            else:
                isections['loneliness'].append(False)
                increment_stats(stats_hourly, i, data, 'non_loneliness', start_hour, isections)
                loneliness_isec = None

        if write_convs:
            human_readable = read_json(
                "./conversation_data/{}/human_readable_messages.json".format(app))

            if loneliness_isec and len(loneliness_isec) > 0:
                conv_readable = human_readable[i]['conversation'].split(" | ")
                # Add message indexes before each message, only for messages that start with 'Human: '
                count = 0
                for j, m in enumerate(conv_readable):
                    if m.startswith('Human: '):
                        conv_readable[j] = str(count) + '. ' + m
                        count += 1

                conversation_dict[human_readable[i]['id'] + '_' + str(human_readable[i]['start_time'])] = {
                    'conversation': conv_readable,
                    '(1) Loneliness (0 = No, 1 = Yes)': 1,
                    '(2) Manual Sentiment (0 = Negative, 1 = Positive, 2 = Neutral)': 0,
                    'Detected Loneliness Terms': loneliness_isec,
                    'human_word_count': human_readable[i]['human_word_amt'],
                    'chatbot_word_count': human_readable[i][
                        'chatbot_word_amt'],
                    'start_time': data[i]['start_time'],
                    'duration_in_mins': data[i]['duration_in_mins']
                }

    # Save conversation_dict into a json file
    if write_convs:
        with open('./conversation_data/{}/results/lonely_conversations.json'.format(app), 'w+') as outfile:
            json.dump(conversation_dict, outfile, indent=4)

    # ------------------------- Finished collecting the data -------------------------#

    # Hide the text in the conversations:
    # data_df['conversation'] = data_df.apply(lambda row: hide_text(row['conversation']), axis=1)
    data_df.to_csv('./{}_data/{}/results/results.csv'.format(
        'conversation' if not is_review else 'review', app))
    
    if is_review: return

    # Print the loneliness percentage
    print("Percentage of conversations that contains loneliness words: {:.2f}%".format(
        data_df['contains_loneliness_llm'].sum() * 100 / data_df.shape[0]))

    if is_review: return # Below are hourly stats, which are not relevant for is_review

    # For hour-by-hour analyses
    for hour in range(0, 24):
        for ikey in stats_hourly[0]['durations'].keys():
            avg_stats_hourly[hour]['durations'][ikey] = np.mean(np.asarray(stats_hourly[hour]['durations'][ikey]))
            avg_stats_hourly[hour]['word_counts']['chatbot'][ikey] = np.mean(np.asarray(stats_hourly[hour]['word_counts']['chatbot'][ikey]))
            avg_stats_hourly[hour]['word_counts']['human'][ikey] = np.mean(np.asarray(stats_hourly[hour]['word_counts']['human'][ikey]))
            avg_stats_hourly[hour]['turns'][ikey] = np.mean(np.asarray(stats_hourly[hour]['turns'][ikey]))

    percentages = {'intersect_percentages': {}}

    avg_durations = {"non_loneliness": [], "loneliness": []}
    avg_turns = copy.deepcopy(avg_durations)
    avg_length = copy.deepcopy(avg_durations)

    write_file = open(
        './conversation_data/{}/results/freq_{}.txt'.format(app, 'human'), 'w+')
    write_results(stats_hourly, percentages, avg_durations,
                  avg_length, avg_turns, write_file, isections)

    return {'percentages': percentages, 'stats_hourly': stats_hourly,
            'avg_stats_hourly': avg_stats_hourly}


# read loneliness_dict.csv and remove all duplicates
def remove_duplicates():
    loneliness_list = csv.reader(open('../loneliness_dict.csv'))
    # convert loneliness_list to a list
    loneliness_list = list(loneliness_list)
    loneliness_list = [l[0] for l in loneliness_list]
    # remove duplicates
    loneliness_list = list(dict.fromkeys(loneliness_list))
    with open("./loneliness_dictionary.csv", "w+") as write_file:
        write_file.write("\n".join(loneliness_list))


def compare_annotations():
    with open('./conversation_data/cleverbot/results/lonely_conversations.json') as json_file:
        data = json.load(json_file)

    with open('./conversation_data/cleverbot/results/lonely_conversations_2.json') as json_file:
        data_2 = json.load(json_file)

    # Cronbach's alpha between the two raters
    df = pd.DataFrame.from_dict(data, orient='index')
    df_2 = pd.DataFrame.from_dict(data_2, orient='index')

    df = df[['(1) Loneliness (0 = No, 1 = Yes)', 'Detected Loneliness Terms']]
    df_2 = df_2[['(1) Loneliness (0 = No, 1 = Yes)',
                 'Detected Loneliness Terms']]
    df.columns = ['rater1', 'detected']
    df['id'] = df.index
    df_2.columns = ['rater2', 'detected']
    df_2['id'] = df_2.index

    # Merge df and df_2 on row names
    df = pd.merge(df, df_2, on='id')

    # Cronbach's alpha
    print(pingouin.cronbach_alpha(df[['rater1', 'rater2']], 'alpha'))

    # Get the common ratings in both dataframes, and print the percentage of 1's
    common = df[df['rater1'] == df['rater2']]
    print("Percentage of 1's in common: ",
          common[common['rater1'] == 1].shape[0] / common.shape[0])

    correct_classifications = {}

    # Now among the common df, read 'detected_x' column. In this column, there are lists of detected loneliness terms.
    # If rater1 == 1, then this is a correct classification. Add it to correct_classifications as key=term_found and value={'correct': value += 1}
    # If rater1 == 0, then this is a wrong classification. Add it to correct_classifications as key=term_found and value={'wrong': value += 1}
    for i, row in common.iterrows():
        if row['rater1'] == 1:
            for term in row['detected_x']:
                if term['term_found'] in correct_classifications.keys():
                    correct_classifications[term['term_found']]['correct'] += 1
                else:
                    correct_classifications[term['term_found']] = {
                        'correct': 1, 'wrong': 0}
        else:
            for term in row['detected_x']:
                if term['term_found'] in correct_classifications.keys():
                    correct_classifications[term['term_found']]['wrong'] += 1
                else:
                    correct_classifications[term['term_found']] = {
                        'correct': 0, 'wrong': 1}

    # Now calculate the accuracy of each term and total occurrences
    for term in correct_classifications.keys():
        correct_classifications[term]['total'] = correct_classifications[term]['correct'] + \
            correct_classifications[term]['wrong']
        correct_classifications[term]['accuracy'] = correct_classifications[term]['correct'] / \
            correct_classifications[term]['total']

    print("*-*-*-*-*-*-*-* Less than 0.8 accuracy: *-*-*-*-*-*-*-*")
    terms_to_remove = []
    # Print all with less than 0.7 accuracy
    for term in correct_classifications.keys():
        if correct_classifications[term]['accuracy'] < 0.8:
            print(term, correct_classifications[term])
            terms_to_remove.append(term)

    # Sort them by total occurrences
    correct_classifications = {k: v for k, v in sorted(
        correct_classifications.items(), key=lambda item: item[1]['total'], reverse=True)}

    # Load loneliness dict and see which terms were not detected
    loneliness_list = clean_words(
        list(csv.reader(open('../loneliness_dict.csv'))))

    print("*-*-*-*-*-*-*-* Terms that were not detected: *-*-*-*-*-*-*-*")

    # Get the terms that were not detected
    for term in loneliness_list:
        if term not in correct_classifications.keys():
            print(term)
            terms_to_remove.append(term)

    # remove terms from loneliness_list
    for term in terms_to_remove:
        loneliness_list.remove(term)

    # Save the loneliness words in a csv file
    with open("./loneliness_dictionary.csv", "w+") as write_file:
        write_file.write("\n".join(loneliness_list))

# Read results.csv of each app in review data and combine them into one csv file with the following columns: review, contains_loneliness, userName, id, app
def combine_reviews():
    # Read results.csv of each app in review data and combine them into one csv file
    apps = ['replika', 'cleverbot', 'igirl', 'simsimi', 'chai', 'chatgpt']
    df = pd.DataFrame()
    for app in apps:
        df_app = pd.read_csv("./review_data/{}/results/results.csv".format(app))
        df_app['app'] = app

        # Select the columns we want
        df_app = df_app[['review', 'contains_loneliness_dict', 'userName', 'id', 'app']]

        # Append the dataframe to the combined dataframe
        df = pd.concat([df, df_app])

        # Print number of reviews in this app
        print("Number of reviews in {}: {}".format(app, df_app.shape[0]))

    # get column names
    print(df.columns)

    # Save the combined dataframe into a new csv file
    df.to_csv('./review_data/combined_reviews.csv', index=False)

    # Print table of contains_loneliness_dict and see how many of these are from which app
    print(pd.crosstab(df['contains_loneliness_dict'], df['app']))

    # print number of rows with contains_loneliness = True
    print("Number of reviews that contains loneliness words: {}".format(
        df[df['contains_loneliness_dict'] == True].shape[0]))
    
    # Randomly sample 100 lonely reviews and 1000 non-lonely reviews
    lonely_reviews = df[df['contains_loneliness_dict'] == True].sample(100)
    non_lonely_reviews = df[df['contains_loneliness_dict'] == False].sample(1000)

    # Combine them
    lonely_reviews = pd.concat([lonely_reviews, non_lonely_reviews])

    # Save the lonely and non-lonely reviews into new csv files
    lonely_reviews.to_csv('./review_data/100_lonely_1000_nolonely_random.csv', index=False)

# Read review_inference_full_data.jsonl and add the 'inference_result' to the matching reviews
# to the reviews of all apps. Find the matching review by looking at the 'id', 'app', and 'username' columns
# The apps are: replika, cleverbot, igirl, simsimi, chai, chatgpt
def add_model_results_to_reviews():
    # Read the review_inference_full_data.jsonl
    with open('./review_data/review_inference_full_data.jsonl', 'r') as json_file:
        data = json_file.readlines()

    # Convert the jsonl file into a list of dictionaries
    data = [json.loads(d) for d in data]

    # Convert the list of dictionaries into a dataframe
    df = pd.DataFrame(data)

    # rename username to userName
    df.rename(columns={'username': 'userName'}, inplace=True)

    apps = ['replika', 'cleverbot', 'igirl', 'simsimi', 'chai', 'chatgpt']
    # Read results.csv of each app and add the 'inference_result' to the matching reviews
    for app in apps:
        results_app = pd.read_csv("./review_data/{}/results/results.csv".format(app))

        # Print number of reviews in this app
        print("Number of reviews in {}: {}".format(app, results_app.shape[0]))

        # Now, select only the current app from df, and merge with df_app
        df_app = df[df['app'] == app]
        df_app = df_app.merge(results_app, on=['id', 'userName'], how='inner')

        # Remove text, first_loneliness_message_index columns
        df_app = df_app.drop(columns=['text'])

        # Rename the inference_result column to contains_loneliness_llm
        df_app.rename(columns={'inference_result': 'contains_loneliness_llm'}, inplace=True)

        # Save df_app into a new csv file
        df_app.to_csv('./review_data/{}/results/results_with_inference.csv'.format(app), index=False)
    

if __name__ == '__main__':
    #add_model_results_to_reviews()
    #combine_reviews()
    #reformat_jsonl()
    # process_reviews(app='chatgpt')
    # read_data()
    # format_messages(app='cleverbot')
    # process_conversations(app='cleverbot')
    # read_data()
    write_stats(app='cleverbot', is_review=True, write_convs=False)
    # select_lonely_reviews()
    # reformat_jsonl()

    # compare_annotations()

    # For reddit posts:
    # process_reddit_posts('posts_upto_2022_12_replika')
    # find_mental_health_words('posts_upto_2022_12_replika_processed')
    # gpt_classify_loneliness("This app is the best. I'm not alone anymore.")
