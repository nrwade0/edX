# -*- coding: utf-8 -*-
"""
nrwade0
3/19/19
"""

# Import collections library
import collections

# import a Tale of Two Cities
file=open('98-0.txt', encoding="utf8")

# opening and reading stopwords file, not file type?
stopwords = set(line.strip() for line in open('stopwords'))

# instantiate wordcount dictionary contains every word in the file
wordcount={}

# For each word in 98-0.txt (in the book), change to lowercase (using lower()),
# and deliminate between words (using split()).
for word in file.read().lower().split():
    
    # Eliminate pesky punctuation and split words.
    word = word.replace(".","")
    word = word.replace(",","")
    word = word.replace("\"","")
    word = word.replace("“","")
    
    # If word does not exists in the dictionary add it,
    # Otherwise increment the word's counter.
    
    # Consideration of stopwords...
    if word not in stopwords:
        if word not in wordcount:
            wordcount[word] = 1
        else:
            wordcount[word] += 1
    
    # No consideration of stopword...
    #if word not in wordcount:
    #    wordcount[word] = 1
    #else:
    #    wordcount[word] += 1


# Transfers wordcount dictionary to a counter 'd' that holds the frequencies of
# each word.
d = collections.Counter(wordcount)

# Pull and print the top 10 most common words from d
for word, count in d.most_common(10):
	print(word, ": ", count)
