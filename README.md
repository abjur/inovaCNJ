# faxinaJud

A plataforma **faxinaJud**, do projeto `{inovaCNJ}`, foi elaborada para resolver duas dores:

1)  Inconsistências precisam ser monitoradas continuamente.

2)  O trabalho de arrumar dados precisa ser distribuído nos tribunais / unidades judiciárias.

Por isso, construímos um *dashboard* interativo que, além de mostrar as principais inconsistências e comparar o desempenho dos tribunais, permite o envio de tabelas arrumadas para validação do CNJ.

A plataforma mostra inconsistências identificadas através de regras e ou modelagem estatística. Até o momento, foram implementadas mais de 15 regras, a partir de uma amostra de 100 mil processos, extraída aleatoriamente da base fornecida para este desafio.

Uma parte das inconsistências já tem uma **proposta de** **solução** para o problema, sendo necessário apenas a verificação das sugestões. Outra parte das inconsistências necessita de **verificação manual**, que idealmente deve ser feita pelos técnicos judiciários dos tribunais de origem.

A plataforma é modular, no sentido de que as inconsistências consideradas podem ser aprimoradas ou, ainda, novas inconsistências podem ser incluídas ao longo do tempo. Dessa forma, é possível fazer um monitoramento contínuo dos problemas da base, tornando-a cada vez melhor.

É possível acessar com dois tipos perfis distintos: **pessoa** **técnica judiciária** e **pessoa** **pesquisadora do CNJ**.

A pessoa técnica judiciária pode realizar as atividades a seguir.

-   Visualizar a posição de seu tribunal perante os demais.

-   Visualizar inconsistências.

-   Fazer o download da planilha de inconsistências.

-   Submeter uma tabela de inconsistências arrumadas. As tabelas submetidas são inseridas em um banco de dados de sugestões.

Já a pessoa pesquisadora do CNJ, além das atividades supracitadas, pode visualizar as inconsistências submetidas para decidir se utilizará as sugestões.

As tabelas de sugestões podem ser internamente utilizadas pelo CNJ para produzir a base de dados arrumada dos processos. Dessa forma, todo o sistema judiciário pode contribuir com arrumações dos dados, para que o DataJud se torne cada vez mais completo e correto.

O aplicativo foi construído de forma completamente open source, usando a linguagem de programação R e o pacote `{shiny}`. A ferramenta já é massivamente utilizada pelo Departamento de Pesquisas Judiciárias do CNJ (DPJ-CNJ), o que torna o aprendizado e transferência de conhecimento eficientes.

# Informações para avaliação da solução

Acessar em navegador Chrome ou Firefox.

Link para acesso à plataforma: <https://abjur.shinyapps.io/inovaCNJ>

Pessoa técnica judiciária:

**Login**: [usuario\@inovacnj.com](mailto:usuario@inovacnj.com)

**Senha**: CNJ!2020

Pessoa pesquisadora do CNJ:

**Login**: [admin\@inovacnj.com](mailto:usuario@inovacnj.com)

**Senha**: CNJ!2020

## Aba 1: Menu principal

![](images/geral.gif)

O menu principal contém 4 (quatro) cards e 1 (um) gráfico e 1 (uma) tabela, descritos a seguir.

-   **Card 1 (ranking)**. Mostra a posição do tribunal com relação aos demais tribunais da mesma justiça.

-   **Card 2** (**índice de qualidade):** Número entre 0 e 100 que indica o grau de qualidade dos dados. Quanto maior o número, melhor é a qualidade. Este índice é calculado a partir da quantidade média de processos com inconsistência no tribunal.

-   **Card 3 (processos com inconsistências)**: Quantidade de processos com alguma inconsistência, baseado na amostra observada para o tribunal.

-   **Card 4 (total de soluções)**: Quantidade total de inconsistência com soluções identificadas, considerando todas as possibilidades da amostra observada. Como um processo pode possuir mais de uma inconsistência (e, com isso, mais de uma solução), pode ser que o valor do Card 4 seja maior que o valor do card 3.

-   **Gráfico (comparação)**: Comparação dos índices de inconsistência na justiça do tribunal selecionado. O tribunal em destaque é o que foi selecionado no filtro lateral.

-   **Tabela (principais inconsistências)**: A tabela mostra todos os tipos de inconsistência identificados com pelo menos um problema.

**Para mudar de tribunal:** Acesse o menu lateral, no canto superior direito, ao lado do botão de log-out.

![](images/menu_lateral.png)

## Aba 2: Inconsistências

![](images/inconsistencias.gif)

A aba de inconsistências apresenta uma série de caixas, cada uma sobre uma possível inconsistência. É a aba principal do aplicativo, a ser utilizada principalmente pelas pessoas técnicas judiciárias.

-   **Botão de download**: Faz o download de uma planilha Excel com todos os processos com algum problema identificado.

    -   Colunas com prefixo `info_`: são as colunas originais da base que geraram a inconsistência.

    -   Colunas com prefixo `inc_`: descrição da inconsistência encontrada no caso.

    -   Colunas com prefixo `sol_`: coluna com possibilidade de solução.

-   **Botão de maximizar**: dá ênfase na inconsistência selecionada, possibilitando outras ações:

    -   **Botão de upload:** Faz o upload de uma base de dados arrumada. A ideia é que a pessoa técnica judiciária faça o download da base e, em seguida, faça as correções nas colunas `info_`. Em seguida, ela faz o upload da base de dados com as correções.
    -   **Botão de submeter**: Faz verificações na base de dados submetida (formato das colunas e da base enviada) e faz o upload em um banco de dados que fica na nuvem.

## Aba 3: Verificação das inconsistências submetidas

![](images/verificador.gif)

Essa aba deve ser utilizada pela pessoa pesquisadora do CNJ para realizar a verificação das informações submetidas pelos tribunais.

A primeira caixa permite o download das bases arrumadas, consolidando todas as correções automáticas e manuais que foram submetidas na plataforma.

A primeira tabela da aba mostra todas as tabelas submetidas. Ao clicar em uma linha, os dados arrumados sugeridos são mostrados na segunda tabela. A base pode ser baixada para inspeção manual e, eventualmente, pode ser usada para fazer uma nova submissão na aba de inconsistências.

As bases validadas ficam em um banco de dados, e podem ser utilizadas pelo CNJ para corrigir as inconsistências do DataJud. No final, o CNJ pode disponibilizar tanto a base bruta do DataJud quanto a base arrumada.

## Aba 4: Validação de dados

![](images/validador_arquivos.gif)

Essa aba pode ser utilizada para verificar as inconsistências de um arquivo .json específico, antes de submetê-lo para o CNJ. Da mesma forma que as abas anteriores, é possível visualizar as inconsistências e suas correções.

# Vídeo

O vídeo de demonstração pode ser acessado [nesse link](https://youtu.be/UP3g0bb0BWM).

# Apresentação

[Link para apresentação (PDF)](misc/cnj_inova.pdf)

# Licença

MIT
