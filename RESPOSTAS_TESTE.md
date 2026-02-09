# Respostas - Teste para Seleção (Desenvolvimento)

---

## TESTE LÓGICO

---

### Questão 1 - Avalie V ou F (A=10, B=2, C=8)

| Item | Expressão | Cálculo | Resultado |
|------|-----------|---------|-----------|
| a) | (A+B) = C | 10 + 2 = 12, C = 8 → 12 ≠ 8 | **Falso** |
| b) | (A−C) = B | 10 − 8 = 2, B = 2 → 2 = 2 | **Verdadeiro** |
| c) | (A×C) < B | 10 × 8 = 80, B = 2 → 80 < 2 | **Falso** |
| d) | (A×B) = A | 10 × 2 = 20, A = 10 → 20 ≠ 10 | **Falso** |
| e) | (A−B) = C | 10 − 2 = 8, C = 8 → 8 = 8 | **Verdadeiro** |

---

### Questão 2 - Avalie V ou F (X=2, Y=3, Z=5)

| Item | Expressão | Passo a passo | Resultado |
|------|-----------|---------------|-----------|
| a) | `(((X+Y)>=Z) and (X>Y))` | (2+3=5 >= 5) = **V** and (2 > 3) = **F** → V and F | **Falso** |
| b) | `(((X+Y)>=Z) or (X>Y))` | (5 >= 5) = **V** or (2 > 3) = **F** → V or F | **Verdadeiro** |
| c) | `((Z>Y) and ((Z-Y)=X))` | (5 > 3) = **V** and (5-3=2 = 2) = **V** → V and V | **Verdadeiro** |
| d) | `((X=Y) or (X<Y))` | (2 = 3) = **F** or (2 < 3) = **V** → F or V | **Verdadeiro** |
| e) | `(((X+Y)=Z) and (Z>Y) and ((X-Y)=Z))` | (5=5)=**V** and (5>3)=**V** and (2-3=-1=5)=**F** → V and V and F | **Falso** |

---

### Questão 3 - Saídas do fluxograma

O fluxograma faz o seguinte:
1. Lê os valores `a` e `b`
2. Testa se `a = 0`
   - **Se sim**: imprime "Não há raízes reais"
   - **Se não**: calcula `x = b / a` e imprime `x`

| Entrada | a | b | Condição a=0? | Saída |
|---------|---|---|---------------|-------|
| Entrada 1 | 3 | 4 | Não | **x = 4/3 = 1.3333** |
| Entrada 2 | 0 | 3 | Sim | **"Não há raízes reais"** |
| Entrada 3 | 3 | 9 | Não | **x = 9/3 = 3** |

---

### Questão 4 - Saídas do pseudocódigo

O pseudocódigo realiza uma **multiplicação por somas sucessivas**:

```
principal()
    ler m, n;
    r = 0;
    enquanto n != 0 faça       ← repete n vezes
        r = r + m;             ← soma m ao acumulador
        n = n - 1;             ← decrementa contador
    fim do enquanto
    imprimir r;                ← resultado = m × n
fim de principal
```

Ou seja: `r = m × n` (usando apenas adição)

| Entrada | m | n | Cálculo | Saída (r) |
|---------|---|---|---------|-----------|
| Entrada 1 | 3 | 5 | 3+3+3+3+3 = 15 | **15** |
| Entrada 2 | 100001 | 0 | Loop não executa (n=0) | **0** |
| Entrada 3 | 15 | 3 | 15+15+15 = 45 | **45** |

---

### Questão 5 - Algoritmo: ler 400 números, imprimir maior, menor e média

```
principal()
    ler numero;
    maior = numero;
    menor = numero;
    soma = numero;

    para i de 2 ate 400 faca
        ler numero;

        se numero > maior entao
            maior = numero;

        se numero < menor entao
            menor = numero;

        soma = soma + numero;
    fim para

    media = soma / 400;

    imprimir "Maior: ", maior;
    imprimir "Menor: ", menor;
    imprimir "Media: ", media;
fim de principal
```

**Explicação:**
- O primeiro número lido é usado como referência inicial para `maior` e `menor`.
- A cada novo número, atualiza o maior e o menor conforme necessário.
- A `soma` acumula todos os valores para calcular a média no final.
- Complexidade: O(n) — percorre a lista uma única vez.

---

### Questão 6 - Algoritmo: ler 6 números e imprimir em ordem crescente

```
principal()
    declarar vetor[6];

    // Leitura dos 6 numeros
    para i de 0 ate 5 faca
        ler vetor[i];
    fim para

    // Ordenacao por Bubble Sort
    para i de 0 ate 4 faca
        para j de 0 ate (4 - i) faca
            se vetor[j] > vetor[j+1] entao
                temp = vetor[j];
                vetor[j] = vetor[j+1];
                vetor[j+1] = temp;
            fim se
        fim para
    fim para

    // Impressao em ordem crescente
    para i de 0 ate 5 faca
        imprimir vetor[i];
    fim para
fim de principal
```

**Explicação:**
- Utiliza **Bubble Sort** por ser simples e adequado para apenas 6 elementos.
- O Bubble Sort compara pares adjacentes e troca quando o anterior é maior.
- A cada passagem do laço externo, o maior elemento "flutua" para a posição correta.
- Para 6 números, executa no máximo 15 comparações — instantâneo.

---

## TESTE PRÁTICO

---

### Questão 1 - Aplicação WEB de Agenda Telefônica

A aplicação foi desenvolvida e está funcional. Veja a documentação completa em [README.md](README.md).

**Requisitos atendidos:**

| Requisito do teste | Implementação |
|-------------------|---------------|
| Cadastrar contatos (nome, idade, telefones) | Tela "Novo Contato" com campos Nome, Idade e seção de telefones |
| Botão de inclusão para contato | Botão "+ Novo Contato" na tela Home |
| Botão de pesquisa para os contatos | Campo de pesquisa + botão "Pesquisar" na tela Home |
| Pesquisar por nome e número de telefone | Campo único que busca por nome OR telefone (SQL com LIKE) |
| Botão de alteração para o contato selecionado | Botão "Editar" (ícone lápis) em cada linha do grid |
| Botão de exclusão para o contato selecionado | Botão "Excluir" (ícone lixeira) no grid e dentro da tela de edição |
| Múltiplos telefones por contato | Seção com campo + botão "Adicionar" + grid com lixeira por linha |
| Estrutura das tabelas conforme especificação | `contato` (ID, NOME, IDADE) e `telefone` (ID, IDCONTATO, NUMERO) |

**Tecnologias utilizadas:**
- **Delphi 12** com framework **D2Bridge** (converte VCL para Web automaticamente)
- **MySQL 8** via **FireDAC**
- Frontend gerado automaticamente (Bootstrap 5) — sem JavaScript manual

---

### Questão 2 - LOG de exclusão

O LOG foi implementado e vai além do solicitado. O sistema registra **todas as alterações** (não apenas exclusões) no arquivo `log_alteracoes.txt`:

**Tipos de eventos registrados:**

| Tipo | Quando é gravado |
|------|-----------------|
| `CRIACAO` | Ao criar um novo contato |
| `EDICAO` | Ao salvar alterações em um contato existente |
| `EXCLUSAO` | Ao excluir um contato (grava nome, idade e todos os telefones) |
| `TEL_ADD` | Ao adicionar um telefone a um contato |
| `TEL_REMOVE` | Ao remover um telefone de um contato |

**Exemplo do conteúdo do arquivo:**

```
[2026-02-08 14:00:00] [CRIACAO] Contato criado - ID: 10 | Nome: Carlos Lima | Idade: 35
[2026-02-08 14:00:15] [TEL_ADD] Telefone adicionado - ContatoID: 10 | Numero: (11) 99999-1234
[2026-02-08 14:01:00] [TEL_ADD] Telefone adicionado - ContatoID: 10 | Numero: (11) 3333-5678
[2026-02-08 14:05:00] [EDICAO] Contato editado - ID: 10 | Nome: Carlos Lima Jr | Idade: 36
[2026-02-08 14:10:00] [TEL_REMOVE] Telefone removido - ContatoID: 10 | Numero: (11) 3333-5678
[2026-02-08 15:00:00] [EXCLUSAO] Contato excluido - ID: 10 | Nome: Carlos Lima Jr | Idade: 36 | Telefones: (11) 99999-1234
```

O arquivo é criado automaticamente na mesma pasta do executável (`bin\log_alteracoes.txt`).

---

### Questão 3 - Classificação de dificuldade

**(X) Médio**

O teste tem um nível equilibrado. A parte lógica exige atenção nas operações booleanas e leitura de fluxogramas, sendo direta para quem pratica lógica de programação. A parte prática demanda conhecimento de CRUD web com banco de dados relacional, o que é um exercício comum mas completo — abrangendo modelagem de dados, backend, frontend e integração.

**Maior dificuldade encontrada:** A linguagem Delphi em si. Por nunca ter trabalhado com ela antes, a sintaxe se mostrou bem diferente do que estou habituado — declarações com `begin/end`, tipagem com `:`, separação de interface/implementation, o sistema de units, e toda a forma de organizar o código no padrão Object Pascal foram desafios iniciais. Além disso, o Delphi não possui suporte nativo para desenvolvimento web, o que trouxe a necessidade de utilizar um framework externo (D2Bridge) para converter os forms VCL em páginas web. Isso adicionou uma camada extra de complexidade, pois além de aprender a linguagem, foi preciso entender o ciclo de vida do framework (ExportD2Bridge, OnPageLoaded, ClickProc, etc.) e seus padrões específicos de desenvolvimento, que não seguem a abordagem convencional de frameworks web tradicionais.
