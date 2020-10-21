# import libraries
# from itertools import product
from joblib import Parallel, delayed
from pathlib import Path
import re, os

from tqdm import tqdm
import pandas as pd, numpy as np

from CNJInova import Parser

# define method to parse all files
def parse_lawsuits(path, rows):
    file = Parser(path)
    movs = file.parse_movs(rows)
    try:
        movs = pd.concat([
            pd.json_normalize(mov, 'movimento', ['file_json', 'rowid'])
            for mov in movs
            ], ignore_index=True
        )
        return movs
    except:
        pass

# define main function
def main():

    # define paths
    PROJ = Path()
    DATA = PROJ / 'dados'
    RAW = DATA / 'brutos'
    PROCESSED = DATA / 'processados'

    # load data from R
    da_incos = pd.read_csv(PROCESSED / 'da_incos.csv', low_memory=False)

    # filter identifiers-only in the dat
    dados = da_incos[['file_json', 'rowid']]
    dados.loc[:,'file_json'] = dados['file_json'].str[3:]
    dados = dados.drop_duplicates()

    # produce list for extracting info from cnj inova
    inova = dados.groupby('file_json')['rowid'].apply(list)
    inova = inova.reset_index()
    inova['rowid'] = inova['rowid'].apply(sorted)
    iterator = inova.itertuples(name=None, index=False)

    # execute loops to read, process and join files
    kwargs = {'n_jobs': -2, 'verbose': 10}
    movs = Parallel(**kwargs)(delayed(parse_lawsuits)(*p) for p in iterator)
    movs = pd.concat(movs, ignore_index=True)

    # save to file
    print(movs.columns)
    movs.to_csv(PROCESSED / 'mov_incos.csv', index=False, quoting=1)

# include execution block
if __name__ == '__main__':
    main()
