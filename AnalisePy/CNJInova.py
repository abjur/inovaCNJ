import json


class Parser:

    """ parse the progress of all CNJ lawsuits """

    def __init__(self, filepath):
        self.fp = filepath

    def parse_dataframe(self):
        with open(self.fp, 'rb') as fp:
            processos = json.load(fp)
        return processos

    def parse_movs(self):
        processos = self.parse_dataframe()
        for row, processo in enumerate(processos):
            yield {
                'file_json': self.fp,
                'rowid': row+1,
                'movimento': processo['movimento']
            }
