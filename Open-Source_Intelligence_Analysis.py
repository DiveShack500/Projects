# OSINT Project: Foreign Influence Detection on X
# -----------------------------------------------------
# Simulates tracking and analyzing foreign disinformation campaigns using X data (from CSV)
# Can be used with any CSV file that contains X data

import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt
import re
import nltk
import ast
from nltk.corpus import stopwords
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import LatentDirichletAllocation
from collections import Counter
from datetime import datetime
import ssl
import certifi
import warnings
from fpdf import FPDF
warnings.filterwarnings("ignore")

ssl_context = ssl.create_default_context(cafile=certifi.where())

try:
    stop_words = set(stopwords.words('english'))
except LookupError:
    nltk.download('stopwords')
    stop_words = set(stopwords.words('english'))

# Load Tweets

def load_tweets_from_csv(file_path):
    df = pd.read_csv(file_path, parse_dates=['Datetime'], engine='python')
    df.rename(columns={
        'Datetime': 'date',
        'Text': 'text',
        'Username': 'username',
        'LikeCount': 'likes'
    }, inplace=True)
    df['followers'] = 0
    return df

# Clean Text

def clean_text(text):
    if not isinstance(text, str):
        return ""
    text = re.sub(r'http\S+', '', text)
    text = re.sub(r'[^a-zA-Z ]', '', text)
    tokens = text.lower().split()
    tokens = [word for word in tokens if word not in stop_words and len(word) > 2]
    return ' '.join(tokens)

# Topic Modeling

def topic_modeling(texts, n_topics=5):
    vectorizer = TfidfVectorizer(max_df=0.9, min_df=2)
    dtm = vectorizer.fit_transform(texts)
    lda = LatentDirichletAllocation(n_components=n_topics, random_state=42)
    lda.fit(dtm)
    topics = []
    for idx, topic in enumerate(lda.components_):
        keywords = [vectorizer.get_feature_names_out()[i] for i in topic.argsort()[-5:]]
        print(f"Topic {idx}:", keywords)
        topics.append(f"Topic {idx}: {', '.join(keywords)}")
    return topics

# Build Mention Network

def build_user_network(df):
    G = nx.Graph()
    for _, row in df.iterrows():
        if pd.isna(row['username']):
            continue
        G.add_node(row['username'], followers=row['followers'])
    top_users = df['username'].dropna().value_counts().nlargest(10).index
    for user in top_users:
        mentions = df[df['username'] == user]['text'].str.extractall(r'@(\w+)')[0].dropna().unique()
        for mentioned in mentions:
            G.add_edge(user, mentioned)
    return G

# Visualization

def visualize_network(G):
    plt.figure(figsize=(10, 7))
    pos = nx.spring_layout(G)
    nx.draw(G, pos, with_labels=True, node_size=800, node_color='lightblue', font_size=10)
    plt.title("User Mention Network")
    plt.savefig("mention_network.png")
    plt.close()

def plot_tweet_volume(df):
    df['date'] = pd.to_datetime(df['date'])
    daily_counts = df['date'].dt.date.value_counts().sort_index()
    daily_counts.plot(kind='line', title='Tweet Volume Over Time', figsize=(10, 4))
    plt.xlabel("Date")
    plt.ylabel("Tweet Count")
    plt.tight_layout()
    plt.savefig("tweet_volume.png")
    plt.close()

# Generate Executive Summary

def executive_summary(df, topics):
    peak_date = pd.to_datetime(df['date'].value_counts().idxmax()).strftime('%Y-%m-%d')
    top_user = df['username'].value_counts().idxmax()

    hashtags = df['hashtag'].dropna().astype(str)
    all_hashtags = []
    for hlist in hashtags:
        try:
            items = ast.literal_eval(hlist)
            all_hashtags.extend(items if isinstance(items, list) else [items])
        except:
            continue
    top_hashtags = [tag for tag, _ in Counter(all_hashtags).most_common(5)]

    mentions = df['text'].str.extractall(r'@(\w+)')[0].value_counts().head(5).index.tolist()
    languages = df['Language'].value_counts().head(3).to_dict()

    summary = f"""
    Executive Intelligence Summary
    ------------------------------
    This report analyzes 5,000 tweets related to the Russia-Ukraine conflict.

    Key Insights:
    - Peak tweet activity occurred on {peak_date}.
    - The most active user was @{top_user}.
    - Most common hashtags: {', '.join(top_hashtags)}
    - Most frequently mentioned accounts: {', '.join('@' + m for m in mentions)}
    - Predominant languages: {', '.join(f"{lang} ({count})" for lang, count in languages.items())}
    - Identified topic clusters:
      {chr(10).join(['  - ' + t for t in topics])}

    Conclusion:
    The data indicates concentrated influence activity around key dates, with repeated messaging patterns and frequent user targeting. Continued monitoring is advised, especially around coordinated mentions and language segmentation.
    """
    print(summary)
    return summary

# Save PDF Report

# PDF generation removed

# Main

if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("Usage: python3 Open-Source_Intelligence_Analysis.py <file.csv>")
        sys.exit(1)

    input_path = sys.argv[1]

    print("Loading tweets from CSV...")
    df = load_tweets_from_csv(input_path).head(5000)
    df['cleaned_text'] = df['text'].apply(clean_text)

    print("Running topic modeling...")
    topics = topic_modeling(df['cleaned_text'].tolist())

    print("Building network graph...")
    G = build_user_network(df)
    visualize_network(G)

    print("Plotting tweet volume...")
    plot_tweet_volume(df)

    print("Generating executive summary...")
    summary_text = executive_summary(df, topics)
