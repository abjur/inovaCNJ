# Py

## Instruções por arquivos

### 01-parse-movements.py
Para rodar o parser de movimentações, você deve ter python3, pip e os pacotes no requirements.txt instalados. Favor rodar do root do repositório.

Você deve obrigatoriamente especificar um arquivo feather para ser lido (--source_path) e opcionalmente pode escolher o nome do arquivo a ser salvo (--dest_path='dados/processados/da_movs.feather' é o default).

```shell
python 'AnalisePy/01-parse-movements.py' --source_path='dados/processados/da_basic_transform.feather'
```

