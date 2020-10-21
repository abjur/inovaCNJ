# import libraries
from joblib import Parallel, delayed
from pathlib import Path
import pandas as pd
import re, os

from AnalisePy.CNJInova import Parser
# from CNJInova import Parser


# define method to parse all files
def parse_lawsuits(path, rows):
    file = Parser(path)
    movs = file.parse_movs(rows)
    movs = pd.concat([
        pd.json_normalize(mov, 'movimento', ['file_json', 'rowid'])
        for mov in movs
        ], ignore_index=True
    )
    return movs

# define main method
def main():

    # define paths
    PROJ = Path()
    DATA = PROJ / 'dados'
    RAW = DATA / 'brutos'
    PROCESSED = DATA / 'processados'

    # load digesto
    fpath = [PROCESSED / f'processos0{i}_movs.csv' for i in range(1,4)]
    dados = pd.concat(
        [pd.read_csv(fp, low_memory=False, dtype=str) for fp in fpath],
        ignore_index=True
    )
    dados['numero_cnj'] = dados['numero_cnj'].str.replace(r'\-|\.', '')

    # load cnj inova
    inova = pd.read_feather(PROCESSED / 'da_basic_transform.feather')
    inova = inova[['file_json', 'rowid', 'numero']]
    inova = inova.dropna(subset=['numero'])

    # extrair os números cnj
    numero_cnj = dados['numero_cnj'].to_list()
    numero_cnj = set(numero_cnj)

    # filter inova lawsuits for which we can recover text
    inova = inova[inova['numero'].isin(numero_cnj)]
    dados = dados[dados['numero_cnj'].isin(inova['numero'])]

    # salvar as join keys e o banco do digesto
    inova.to_csv(PROCESSED / 'join_keys.csv', index=False)
    dados.to_csv(PROCESSED / 'movs_texto.csv', index=False, quoting=1)

    # produce list for extracting info from cnj inova
    inova = inova[['file_json', 'rowid']]
    inova = inova.groupby('file_json')['rowid'].apply(list)
    inova = inova.reset_index()
    inova['file_json'] = inova['file_json'].str[3:]
    inova = inova.itertuples(name=None, index=False)

    # execute loops to read, process and join files
    kwargs = {'n_jobs': -2, 'verbose': 10}
    movs = Parallel(**kwargs)(delayed(parse_lawsuits)(*p) for p in inova)
    movs = pd.concat(movs, ignore_index=True)
    movs = movs.astype(str)
    movs.to_csv(PROCESSED / 'movs_inova.csv', index=False, quoting=1)

# execute block
if __name__ == '__main__':
    main()
