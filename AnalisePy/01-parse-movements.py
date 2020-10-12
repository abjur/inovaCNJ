# import standard libraries
from joblib import Parallel, delayed
from pathlib import Path
import argparse, json, re, os

# import third-party libraries
import pandas as pd

# import local libraries
from CNJInova import Parser

# define method to parse all files
def parse_lawsuits(path):
    file = Parser(path)
    movs = file.parse_movs()
    movs = pd.concat([
        pd.json_normalize(mov, 'movimento', ['file_json', 'rowid'])
        for mov in movs
        ], ignore_index=True
    )
    return movs

# define main method
def main():

    # define arg parser
    parser = argparse.ArgumentParser()
    parser.add_argument('--source_path', required=True)
    parser.add_argument('--dest_path', default='da_movs.feather')
    args = vars(parser.parse_args())

    # define path
    PROJ = Path()
    DATA = PROJ / 'dados'
    RAW = DATA / 'brutos'
    PROCESSED = DATA / 'processados'

    # load main data
    dados = pd.read_feather(args['source_path'])
    dados = dados.drop_duplicates('file_json')
    paths = list(set(dados['file_json'].str[3:].to_list()))[:10]

    # execute loops to read, process and join files
    kwargs = {'n_jobs': -2, 'verbose': 10}
    movs = Parallel(**kwargs)(delayed(parse_lawsuits)(p) for p in paths)
    movs = pd.concat(movs, ignore_index=True)
    movs = movs.astype(str)
    movs.to_feather(PROCESSED / args['dest_path'])

# add execution block
if __name__ == '__main__':
    main()
