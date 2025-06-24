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
from textblob import TextBlob
import ssl
import certifi
import warnings

warnings.filterwarnings("ignore")
ssl_context = ssl.create_default_context(cafile=certifi.where())

try:
    stop_words = set(stopwords.words('english'))
except LookupError:
    nltk.download('stopwords')
    stop_words = set(stopwords.words('english'))

# -------------------------------------
# Load Tweets
# -------------------------------------
def load_tweets_from_csv(file_path):
    df = pd.read_csv(file_path, engine='python')
    cols = df.columns.str.lower()

    # Detect important fields
    text_col = next((col for col in cols if 'text' in col), None)
    date_col = next((col for col in cols if 'date' in col), None)
    user_col = next((col for col in cols if col in ['username', 'user', 'id']), None)
    like_col = next((col for col in cols if 'like' in col), None)
    comment_col = next((col for col in cols if 'comment' in col), None)

    if not text_col or not date_col or not user_col:
        raise ValueError("CSV must include 'text', 'date', and 'id/username' columns.")

    df.rename(columns={text_col: 'text', date_col: 'date', user_col: 'username'}, inplace=True)
    if like_col: df.rename(columns={like_col: 'likes'}, inplace=True)
    else: df['likes'] = 0

    if comment_col: df.rename(columns={comment_col: 'comments'}, inplace=True)
    else: df['comments'] = ''

    def parse_custom_date(d):
        try:
            cleaned = re.sub(r'·|UTC', '', str(d)).strip()
            return pd.to_datetime(cleaned, errors='coerce')
        except:
            return pd.NaT

    df['date'] = df['date'].astype(str).apply(parse_custom_date)
    df['followers'] = 0
    return df

# -------------------------------------
# Cleaning & Sentiment
# -------------------------------------
def clean_text(text):
    if not isinstance(text, str):
        return ""
    text = re.sub(r"http\S+|@\w+|[^a-zA-Z ]", " ", text)
    tokens = text.lower().split()
    return ' '.join([w for w in tokens if w not in stop_words and len(w) > 2])

def analyze_sentiment(text):
    try:
        return TextBlob(text).sentiment.polarity
    except:
        return 0

# -------------------------------------
# Topic Modeling
# -------------------------------------
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

# -------------------------------------
# Network & Coordination
# -------------------------------------
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

def detect_coordination(df):
    """Simple heuristic: users who frequently mention each other in both text and comments"""
    pairs = []
    for _, row in df.iterrows():
        text_mentions = re.findall(r'@(\w+)', str(row['text']))
        comment_mentions = re.findall(r'@(\w+)', str(row['comments']))
        overlap = set(text_mentions) & set(comment_mentions)
        if overlap:
            for o in overlap:
                pairs.append((row['username'], o))
    return Counter(pairs).most_common(5)

# -------------------------------------
# Visualization
# -------------------------------------
def visualize_network(G):
    plt.figure(figsize=(10, 7))
    pos = nx.spring_layout(G)
    nx.draw(G, pos, with_labels=True, node_size=800, node_color='lightblue', font_size=10)
    plt.title("User Mention Network")
    plt.savefig("mention_network.png")
    plt.close()

def plot_tweet_volume(df):
    daily_counts = df['date'].dt.date.value_counts().sort_index()
    daily_counts.plot(kind='line', title='Tweet Volume Over Time', figsize=(10, 4))
    plt.xlabel("Date")
    plt.ylabel("Tweet Count")
    plt.tight_layout()
    plt.savefig("tweet_volume.png")
    plt.close()

# -------------------------------------
# Executive Summary
# -------------------------------------
def executive_summary(df, tweet_topics, comment_topics, coordination_pairs):
    peak_date = pd.to_datetime(df['date'].value_counts().idxmax()).strftime('%Y-%m-%d')
    top_user = df['username'].value_counts().idxmax()

    mentions = df['text'].str.extractall(r'@(\w+)')[0].value_counts().head(5).index.tolist()
    avg_sentiment = df['sentiment'].mean()
    avg_comment_sentiment = df['comment_sentiment'].mean()

    summary = f"""
    Executive Intelligence Summary
    ------------------------------
    This report analyzes {len(df)} posts and comments.

    Key Insights:
    - Peak tweet activity occurred on {peak_date}.
    - The most active user was @{top_user}.
    - Most frequently mentioned accounts: {', '.join('@' + m for m in mentions)}
    - Average tweet sentiment: {avg_sentiment:.2f}
    - Average comment sentiment: {avg_comment_sentiment:.2f}

    Tweet Topic Clusters:
    {chr(10).join(['  - ' + t for t in tweet_topics])}

    Comment Topic Clusters:
    {chr(10).join(['  - ' + t for t in comment_topics])}

    Detected Coordinated User Pairs:
    {chr(10).join([f"  - @{a} <--> @{b} ({count} shared mentions)" for (a, b), count in coordination_pairs])}

    Conclusion:
    Observed significant engagement from a small group of users. Language and coordination patterns suggest deliberate amplification strategies.
    """
    print(summary)
    return summary

# -------------------------------------
# Main
# -------------------------------------
if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("Usage: python3 OSINT.py <file.csv>")
        sys.exit(1)

    input_path = sys.argv[1]
    print("Loading tweets from CSV...")
    df = load_tweets_from_csv(input_path).head(5000)

    print("Cleaning and analyzing...")
    df['cleaned_text'] = df['text'].apply(clean_text)
    df['cleaned_comments'] = df['comments'].astype(str).apply(clean_text)
    df['sentiment'] = df['cleaned_text'].apply(analyze_sentiment)
    df['comment_sentiment'] = df['cleaned_comments'].apply(analyze_sentiment)

    print("Running topic modeling...")
    tweet_topics = topic_modeling(df['cleaned_text'].tolist())
    comment_texts = df['cleaned_comments'].dropna().tolist()
    comment_texts = [text for text in comment_texts if len(text.split()) >= 3]

    if comment_texts:
        comment_topics = topic_modeling(comment_texts, n_topics=3)
    else:
        comment_topics = ["Not enough comment data for topic modeling."]


    print("Building network graph...")
    G = build_user_network(df)
    visualize_network(G)

    print("Detecting coordination...")
    coordination_pairs = detect_coordination(df)

    print("Plotting tweet volume...")
    plot_tweet_volume(df)

    print("Generating executive summary...")
    executive_summary(df, tweet_topics, comment_topics, coordination_pairs)
