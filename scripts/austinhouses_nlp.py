# used chatGPT to convert our R script to python
# have not tested as of Aug 6 11:25pm

import pandas as pd
import numpy as np
import re
from datetime import datetime, timedelta
from sklearn.preprocessing import OneHotEncoder

# Load dataset
housing_data = pd.read_csv('PredictionContest/austinhouses.csv')

# Lowercase and basic cleanup
housing_data['description'] = housing_data['description'].str.lower().fillna("")

# Define stopwords
from sklearn.feature_extraction.text import ENGLISH_STOP_WORDS
custom_ignore_words = {
    "austin", "home", "house", "texas", "kitchen", "bedrooms", "2", "3", "master",
    "living", "family", "bath", "bathrooms", "ft", "floor", "bedroom", "sq", "4", "tx", "mo"
}
stop_words = ENGLISH_STOP_WORDS.union(custom_ignore_words)

# Tokenization and word counting
from collections import Counter
from sklearn.feature_extraction.text import CountVectorizer

vectorizer = CountVectorizer(stop_words=stop_words)
X_counts = vectorizer.fit_transform(housing_data['description'])
word_counts = pd.DataFrame({
    'word': vectorizer.get_feature_names_out(),
    'count': X_counts.toarray().sum(axis=0)
})
top_words = word_counts.sort_values(by='count', ascending=False).head(25)
print(top_words)

# Word lists from Zillow
word_lists = {
    'good': [
        'luxurious', 'captivating', 'impeccable', 'stainless', 'basketball',
        'landscaped', 'granite', 'pergola', 'remodel', 'beautiful',
        'gentle', 'spotless', 'tile', 'upgraded', 'updated', 'greenbelt'
    ],
    'bad': [
        'fixer', 'charming', 'motivated seller', 'cozy', 'tlc',
        'cosmetic', 'investment', 'investor', 'potential', 'bargain',
        'opportunity', 'nice', 'bones', 'sold as is'
    ]
}

def count_keywords(texts, keywords):
    return [sum(bool(re.search(rf"\b{re.escape(word)}", txt, flags=re.IGNORECASE)) for txt in texts) for word in keywords]

good_counts = count_keywords(housing_data['description'], word_lists['good'])
bad_counts = count_keywords(housing_data['description'], word_lists['bad'])

good_df = pd.DataFrame({'word': word_lists['good'], 'count': good_counts}).sort_values(by='count', ascending=False)
bad_df = pd.DataFrame({'word': word_lists['bad'], 'count': bad_counts}).sort_values(by='count', ascending=False)

print(good_df)
print(bad_df)

# Trimmed word list
word_lists = {
    'good': ['luxurious', 'stainless', 'basketball', 'landscaped', 
             'granite', 'pergola', 'remodel', 'beautiful', 
             'tile', 'upgraded', 'updated', 'greenbelt'],
    'bad': ['charming', 'cozy', 'investment', 'investor', 
            'potential', 'opportunity', 'nice']
}

# One-hot encoding
search_words = {f"good_{w}": w for w in word_lists['good']}
search_words.update({f"bad_{w}": w for w in word_lists['bad']})

for colname, word in search_words.items():
    pattern = re.compile(re.escape(word), flags=re.IGNORECASE)
    housing_data[colname] = housing_data['description'].apply(lambda x: 1 if pattern.search(x) else 0)

# Cleanup + Feature Engineering
housing_data['latest_saledate'] = pd.to_datetime(housing_data['latest_saledate'], errors='coerce')
ref_date = datetime.today() - timedelta(days=(3 * 365))
housing_data['days_since_sale'] = (ref_date - housing_data['latest_saledate']).dt.days
housing_data['yearsOld'] = 2022 - housing_data['yearBuilt']

housing_data = housing_data.drop(columns=[
    'streetAddress', 'latest_saledate', 'latest_salemonth', 'latest_saleyear',
    'yearBuilt', 'homeType', 'description'
], errors='ignore')

# One-hot encode zipcode
zipcode_dummies = pd.get_dummies(housing_data['zipcode'], prefix='zipcode')
rare_zipcodes = ["zipcode78734", "zipcode78742", "zipcode78652", "zipcode78719", "zipcode78738"]
zipcode_dummies = zipcode_dummies.drop(columns=[z for z in rare_zipcodes if z in zipcode_dummies.columns])
housing_data = housing_data.drop(columns='zipcode')
housing_data = pd.concat([housing_data, zipcode_dummies], axis=1)

# Transformed features
housing_data['log_lotSizeSqFt'] = np.log1p(housing_data['lotSizeSqFt'])
housing_data['log_livingAreaSqFt'] = np.log1p(housing_data['livingAreaSqFt'])
housing_data['log_avgSchoolSize'] = np.log1p(housing_data['avgSchoolSize'])
housing_data['yearsOld_sq'] = housing_data['yearsOld'] ** 2
housing_data['avgSchoolRating_sq'] = housing_data['avgSchoolRating'] ** 2

# Save binary NLP version
housing_data_binary_NLP = housing_data.copy()

# Create numeric NLP version
good_cols = [col for col in housing_data.columns if col.startswith("good_")]
bad_cols = [col for col in housing_data.columns if col.startswith("bad_")]
housing_data_numeric_NLP = housing_data.copy()
housing_data_numeric_NLP['word_count_good'] = housing_data[good_cols].sum(axis=1)
housing_data_numeric_NLP['word_count_bad'] = housing_data[bad_cols].sum(axis=1)
housing_data_numeric_NLP = housing_data_numeric_NLP.drop(columns=good_cols + bad_cols)

# Export
housing_data_binary_NLP.to_csv("housing_data_binary_NLP.csv", index=False)
housing_data_numeric_NLP.to_csv("housing_data_numeric_NLP.csv", index=False)
