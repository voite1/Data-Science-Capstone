'''
Created on Mar 23, 2016

@author: db345c
'''

import string
import re

cList = {
  "ain't": "am not",
  "aren't": "are not",
  "can't": "cannot",
  "can't've": "cannot have",
  "'cause": "because",
  "could've": "could have",
  "couldn't": "could not",
  "couldn't've": "could not have",
  "didn't": "did not",
  "doesn't": "does not",
  "don't": "do not",
  "hadn't": "had not",
  "hadn't've": "had not have",
  "hasn't": "has not",
  "haven't": "have not",
  "he'd": "he would",
  "he'd've": "he would have",
  "he'll": "he will",
  "he'll've": "he will have",
  "he's": "he is",
  "how'd": "how did",
  "how'd'y": "how do you",
  "how'll": "how will",
  "how's": "how is",
  "I'd": "I would",
  "I'd've": "I would have",
  "I'll": "I will",
  "I'll've": "I will have",
  "I'm": "I am",
  "I've": "I have",
  "isn't": "is not",
  "it'd": "it had",
  "it'd've": "it would have",
  "it'll": "it will",
  "it'll've": "it will have",
  "it's": "it is",
  "let's": "let us",
  "ma'am": "madam",
  "mayn't": "may not",
  "might've": "might have",
  "mightn't": "might not",
  "mightn't've": "might not have",
  "must've": "must have",
  "mustn't": "must not",
  "mustn't've": "must not have",
  "needn't": "need not",
  "needn't've": "need not have",
  "o'clock": "of the clock",
  "oughtn't": "ought not",
  "oughtn't've": "ought not have",
  "shan't": "shall not",
  "sha'n't": "shall not",
  "shan't've": "shall not have",
  "she'd": "she would",
  "she'd've": "she would have",
  "she'll": "she will",
  "she'll've": "she will have",
  "she's": "she is",
  "should've": "should have",
  "shouldn't": "should not",
  "shouldn't've": "should not have",
  "so've": "so have",
  "so's": "so is",
  "that'd": "that would",
  "that'd've": "that would have",
  "that's": "that is",
  "there'd": "there had",
  "there'd've": "there would have",
  "there's": "there is",
  "they'd": "they would",
  "they'd've": "they would have",
  "they'll": "they will",
  "they'll've": "they will have",
  "they're": "they are",
  "they've": "they have",
  "to've": "to have",
  "wasn't": "was not",
  "we'd": "we had",
  "we'd've": "we would have",
  "we'll": "we will",
  "we'll've": "we will have",
  "we're": "we are",
  "we've": "we have",
  "weren't": "were not",
  "what'll": "what will",
  "what'll've": "what will have",
  "what're": "what are",
  "what's": "what is",
  "what've": "what have",
  "when's": "when is",
  "when've": "when have",
  "where'd": "where did",
  "where's": "where is",
  "where've": "where have",
  "who'll": "who will",
  "who'll've": "who will have",
  "who's": "who is",
  "who've": "who have",
  "why's": "why is",
  "why've": "why have",
  "will've": "will have",
  "won't": "will not",
  "won't've": "will not have",
  "would've": "would have",
  "wouldn't": "would not",
  "wouldn't've": "would not have",
  "y'all": "you all",
  "y'alls": "you alls",
  "y'all'd": "you all would",
  "y'all'd've": "you all would have",
  "y'all're": "you all are",
  "y'all've": "you all have",
  "you'd": "you had",
  "you'd've": "you would have",
  "you'll": "you you will",
  "you'll've": "you you will have",
  "you're": "you are",
  "you've": "you have"
}

# compile reg ex
# https://gist.github.com/nealrs/96342d8231b75cf4bb82
c_re = re.compile('(%s)' % '|'.join(cList.keys()))

def expandContractions(text, c_re=c_re):
    def replace(match):
        return cList[match.group(0)]
    return c_re.sub(replace, text)

replace_punctuation = string.maketrans(string.punctuation, ' '*len(string.punctuation))

lines = []

with open ("./subset/blogs.txt") as b:
    data = b.readlines()
    for i in data:
        i = i.strip()
        if not i == "NA":
            i = expandContractions(i)
            i = i.replace("'", " ")
            i = i.translate(replace_punctuation)
            i = " ".join(re.findall("[a-zA-Z]+", i))
            i = i.lower()
            i = i.strip()
            lines.append(i)

print "After processing blogs.txt", len(lines)

with open ("./subset/news.txt") as n:
    data = n.readlines()
    for i in data:
        i = i.strip()
        if not i == "NA":
            i = expandContractions(i)
            i = i.replace("'", " ")
            i = i.translate(replace_punctuation)
            i = " ".join(re.findall("[a-zA-Z]+", i))
            i = i.lower()
            i = i.strip()
            lines.append(i)

print "After processing news.txt", len(lines)

with open ("./subset/twitter.txt") as t:
    data = t.readlines()
    for i in data:
        i = i.strip()
        if not i == "NA":
            i = expandContractions(i)
            i = i.replace("'", " ")
            i = i.translate(replace_punctuation)
            i = " ".join(re.findall("[a-zA-Z]+", i))
            i = i.lower()
            i = i.strip()
            lines.append(i)

print "After processing twitter.txt", len(lines)


dictionary = {}

count = 0
for i in lines:
    line = i.split()
    count = count + 1
    for word in line:
        word = word.strip()
        
        if word in dictionary:
            dictionary[word] = dictionary[word] + 1
        else:
            dictionary[word] = 1
        
print "Count of lines processed", count
print "Number of terms in Dictionary", len(dictionary)

gt2 = {}

for word in dictionary:
    if dictionary[word] >= 2:
        gt2[word] = dictionary[word]

print "2 or more instances", len(gt2)

# Removing profanities
prof = []
with open("profanity_list.txt") as p:
    prof = p.readlines()
    
for pr in prof:
    pr = pr.strip()
    if pr in gt2.keys():
        del gt2[pr]

print "After removing profanity language", len(gt2)

    
print gt2["i"]
print gt2["r"]
print gt2["of"]
print gt2["is"]
print gt2["that"]
print gt2["context"]
print gt2["are"]

with open("unigrams.csv", "w") as out:
    out.write("w1,rank\n")
    for i in gt2:
        line = i + "," + str(gt2[i]) + "\n"
        out.write(line)

print "File is ready"
