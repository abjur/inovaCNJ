import json
import random


class Parser:

    """ parse the progress of all CNJ lawsuits """

    def __init__(self, filepath):
        self.fp = filepath

    def parse_dataframe(self):
        with open(self.fp, 'rb') as fp:
            processos = json.load(fp)
        return processos

    def parse_movs(self, rows, n_sample=100):
        processos = self.parse_dataframe()
        if not rows:
            rows = random.sample(range(len(processos)), n_sample)
        for row, processo in enumerate(processos):
            if row in rows:
                yield {
                    'file_json': self.fp,
                    'rowid': row+1,
                    'movimento': processo['movimento']
                }
