#### Importando librerías

import os
import time
import pickle

from operator import itemgetter

# Expresiones regulares
import re

## Librerias de manipulacion y analisis
import pandas as pd
import numpy as np
import string

##Librerias de lenguaje natural
from nltk.corpus import stopwords as sw
from nltk.corpus import wordnet as wn
from nltk import wordpunct_tokenize
from nltk import sent_tokenize
from nltk import pos_tag

## Librerias de sklearn
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import LabelEncoder
from sklearn.linear_model import SGDClassifier
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.metrics import classification_report as clsr
from sklearn.feature_extraction.text import TfidVectorizer
from sklearn.cross_validation import train_test_split as tts ## Será deprecated



## Realizando el modelo














