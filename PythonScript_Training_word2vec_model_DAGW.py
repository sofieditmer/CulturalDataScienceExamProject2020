# Importing modules
import gensim.models
import gensim.models.word2vec
import os
from gensim import utils

class MySentences(object):
     def __init__(self, dirname):
         self.dirname = dirname

     def __iter__(self):
         for fname in os.listdir(self.dirname):
             for line in open(os.path.join(self.dirname, fname), errors='ignore'): # We noticed that there are emojies and we do not want them (therefore we say errors = ignore)
                 yield utils.simple_preprocess(line)

sentences = MySentences('/Users/jdi/PycharmProjects/BachelorProject/Preprocessed_Data/Files')
model = gensim.models.Word2Vec(sentences, size = 500, window = 5, sg = 1) # sg = 1 means that it uses skipgram and not CBOW

# Save model
model.wv.save_word2vec_format('semantic_model_DAGW_500dm_sg.bin', binary=True)

# Load model and test it
model = gensim.models.KeyedVectors.load_word2vec_format("semantic_model_DAGW_500dm_sg.wv.bin", binary=True)

# Find the number of words in the vocabulary
print(len(model.vocab))




