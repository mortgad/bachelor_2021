import spacy
import pandas as pd

# Loading in the data
file_path_gustav = "/Users/gustavhelms/Documents/Cognitive Science/5_semester/Bachelor/political_polarization/Folketinget-Scraping/data/folketinget_2019_2021_raw.csv"
df = pd.read_csv(file_path_gustav)

# Loading the model, disabling all modules that are not relevant for the lemmatization
nlp = spacy.load("da_core_news_sm", disable = ["ner", "parser"])

# lemmatization loop
lemmatized = []
for speech in df["text"]:
    doc = nlp(speech)
    lemmatized.append(" ".join([token.lemma_ for token in doc]))

# Appending the lemmatized text to the data frame
df["lemmatized"] = lemmatized

df.to_csv("folketinget_2019_2021_lemmatized.csv",
                sep=";", index=False)