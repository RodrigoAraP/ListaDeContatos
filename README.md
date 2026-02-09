# Agenda de Contatos

Aplicação **web** de agenda telefônica desenvolvida com **Delphi 12 + D2Bridge Framework + MySQL 8**.

> **Respostas do Teste de Seleção:** [RESPOSTAS_TESTE.md](RESPOSTAS_TESTE.md)

---

## Visão Geral

O sistema permite cadastrar, editar, pesquisar e excluir contatos com múltiplos telefones, tudo via navegador. O D2Bridge converte forms Delphi em páginas web (Bootstrap 5) automaticamente — sem necessidade de JavaScript manual.

**Duas telas:**
- **Home** — listagem com pesquisa (por nome ou telefone), botões de editar/excluir por linha
- **Contato** — formulário de criar/editar contato, gerenciamento de telefones com lixeira por linha

---

## Arquitetura e Fluxo de Execução

```
Navegador (HTTP :8080)
     │
     ▼
D2Bridge Server (Unit_D2BridgeServer)
     │
     ├── Nova sessão → Unit_ServerController.OnNewSession
     │       ├── Cria TAgendaContatosSession (dados da sessão do usuário)
     │       └── Cria TDM (DataModule com conexão MySQL)
     │
     ├── PrimaryFormClass = TFormHome (primeira tela carregada)
     │       ├── ExportD2Bridge → monta layout HTML (feito UMA VEZ)
     │       ├── InitControlsD2Bridge → configura grid, colunas, botões
     │       └── DoPageLoaded → recarrega dados (cada vez que a página é exibida)
     │
     └── Navegação → TFormContato.Show
             ├── ExportD2Bridge → monta layout do formulário
             ├── InitControlsD2Bridge → configura grid de telefones, lixeira
             └── DoPageLoaded → abre contato existente ou prepara novo
```

---

## Estrutura do Projeto

```
Lista De Contato/
├── AgendaContatos.dpr              ← Arquivo principal do projeto
├── AgendaContatos.dproj            ← Projeto Delphi (Win64, define D2BRIDGE)
├── database.sql                    ← Script SQL (tabelas + dados de exemplo)
├── README.md                       ← Este arquivo
├── RESPOSTAS_TESTE.md              ← Respostas do teste lógico e prático
│
├── bin/                            ← Pasta de saída (executável + DLLs)
│   ├── Config.ini                  ← Conexão MySQL (host, porta, user, senha)
│   ├── libmysql.dll                ← Driver MySQL 64-bit
│   ├── libcrypto-3-x64.dll         ← OpenSSL
│   ├── libssl-3-x64.dll            ← OpenSSL
│   ├── wwwroot/                    ← Arquivos estáticos (gerado pelo D2Bridge)
│   └── log_alteracoes.txt          ← Log de todas as operações (gerado em runtime)
│
└── src/
    ├── Unit_D2BridgeServer.pas     ← Form GUI para iniciar/parar o servidor
    ├── Unit_ServerController.pas   ← Controlador: cria sessão + DataModule
    ├── AgendaContatos_Session.pas  ← Dados da sessão (contato selecionado)
    ├── Unit_DM.pas                 ← DataModule: conexão + todas as queries SQL
    ├── Unit_FormHome.pas           ← Tela Home (listagem + pesquisa)
    ├── Unit_FormContato.pas        ← Tela Contato (criar/editar + telefones)
    └── Unit_Log.pas                ← Utilitário de log com tipos de evento
```

---

## Como Cada Arquivo Funciona

### `AgendaContatos.dpr` — Ponto de entrada

Registra todas as units e cria o form principal (`TFormServer`). No Delphi, o `.dpr` é equivalente ao `main()` de outras linguagens.

### `Unit_D2BridgeServer.pas` — Servidor D2Bridge

Form com botão "Start" e um MemoLog. Ao clicar:
1. Define `PrimaryFormClass := TFormHome` (tela inicial do usuário)
2. Ativa `IncludeJQuery := True` (necessário para componentes D2Bridge)
3. Chama `StartServer` — o D2Bridge inicia um servidor HTTP na porta 8080

### `Unit_ServerController.pas` — Controlador de Sessões

Quando um novo usuário acessa o sistema:
1. `OnNewSession` é disparado
2. Cria uma instância de `TAgendaContatosSession` (armazenada no D2Bridge)
3. Cria uma instância de `TDM` (DataModule separado por sessão)

A função global `AppSession` retorna a sessão do usuário atual.

### `AgendaContatos_Session.pas` — Sessão do Usuário

Classe simples com dois campos:
- `ContatoSelecionadoID` — ID do contato sendo editado (0 = novo)
- `ContatoSelecionadoNome` — Nome do contato (usado no log)

Cada aba do navegador tem sua própria sessão.

### `Unit_DM.pas` — DataModule (Acesso ao Banco)

Coração do sistema. Contém:

| Componente | Função |
|-----------|--------|
| `FDConnection` | Conexão lazy com MySQL (lê Config.ini) |
| `QryPesquisa` + `DSPesquisa` | Query de listagem com GROUP_CONCAT para telefones |
| `QryContato` + `DSContato` | Query de um contato (CachedUpdates para edição) |
| `QryTelefones` + `DSTelefones` | Query de telefones do contato |
| `QryExec` | Query auxiliar para INSERT/DELETE direto |

**Métodos principais:**

| Método | O que faz |
|--------|-----------|
| `EnsureConnected` | Conecta ao MySQL na primeira chamada (lazy) |
| `PesquisarContatos(texto)` | Busca por nome OR telefone usando `LIKE` + `GROUP_CONCAT` |
| `AbrirContato(id)` | Carrega um contato em `QryContato` |
| `AbrirTelefones(idContato)` | Carrega telefones em `QryTelefones` |
| `InserirContato` | Prepara query vazia para Append (novo contato) |
| `SalvarContato` | Grava contato via `Post` + `ApplyUpdates` |
| `SalvarContatoRetornarID` | Salva e retorna o ID gerado (`LAST_INSERT_ID`) |
| `ExcluirContato(id)` | `DELETE FROM contato WHERE ID = :id` |
| `InserirTelefoneSQL(idContato, numero)` | `INSERT INTO telefone` direto |
| `ExcluirTelefonePorID(id)` | `DELETE FROM telefone WHERE ID = :id` |

**Busca unificada (nome + telefone):**

```sql
SELECT c.ID, c.NOME, c.IDADE,
       GROUP_CONCAT(t.NUMERO SEPARATOR ', ') AS TELEFONES
FROM contato c
LEFT JOIN telefone t ON t.IDCONTATO = c.ID
WHERE c.NOME LIKE '%texto%'
   OR c.ID IN (SELECT IDCONTATO FROM telefone WHERE NUMERO LIKE '%texto%')
GROUP BY c.ID, c.NOME, c.IDADE
ORDER BY c.NOME
```

### `Unit_FormHome.pas` — Tela Home

**Layout (montado em `ExportD2Bridge`):**
```
┌─────────────────────── container (max-width: 1600px) ──────────────────────┐
│  [h2: Agenda de Contatos]                                                   │
│  [ Campo de Pesquisa (col-8) ][ Pesquisar ]              [ + Novo Contato ] │
│  ┌──────────────────────────────────────────────────────────────────────────┐│
│  │ NOME          │ IDADE │ TELEFONES              │ AÇÕES                  ││
│  │ Maria Silva   │ 30    │ (11) 99999-0001, ...   │ [Editar] [Excluir]     ││
│  │ João Santos   │ 25    │ (21) 88888-0002        │ [Editar] [Excluir]     ││
│  └──────────────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────────────┘
```

**Ciclo de vida:**
1. `ExportD2Bridge` — Executa UMA VEZ por sessão. Conecta o DataSource à query, abre a query e monta o layout HTML com componentes D2Bridge.
2. `InitControlsD2Bridge` — Configura colunas do grid (esconde ID, define larguras), adiciona coluna "Ações" com botões Editar/Excluir via `ClickProc`.
3. `DoPageLoaded` — Executa TODA VEZ que a página é exibida. Recarrega os contatos do banco.

**Navegação para edição:**
- Ao clicar "Editar": grava o ID na sessão → chama `FormContato.Show`
- Ao clicar "Excluir": grava log → exclui do banco → recarrega lista
- Ao clicar "Novo Contato": limpa sessão (ID=0) → chama `FormContato.Show`

### `Unit_FormContato.pas` — Tela Contato (Criar/Editar)

**Layout:**
```
┌─────────────────────── container (max-width: 1600px) ──────────────────────┐
│  [h2: Novo Contato / Editar: Nome]                                          │
│  [ Nome (col-6) ]  [ Idade (col-2) ]                                       │
│  [ Telefone (col-4) ]  [ + Adicionar ]                                     │
│  ┌───────────────────────────┐                                              │
│  │ NÚMERO         │ AÇÕES    │                                              │
│  │ (11) 99999-0001│ [🗑️]     │                                              │
│  │ (11) 3333-0001 │ [🗑️]     │                                              │
│  └───────────────────────────┘                                              │
│  [ Salvar ]  [ Voltar ]                          [ Excluir Contato ]        │
└─────────────────────────────────────────────────────────────────────────────┘
```

**Fluxo para novo contato:**
1. Usuário preenche Nome e Idade
2. Ao clicar "Adicionar" telefone → `GarantirContatoSalvo`:
   - Se contato ainda não foi salvo, salva primeiro (`SalvarContatoRetornarID`)
   - Grava log de criação
   - Depois insere o telefone via `InserirTelefoneSQL`
3. Ao clicar "Salvar" → grava log de criação (se novo) ou edição → volta para Home

**Fluxo para edição:**
1. `DoPageLoaded` carrega contato + telefones do banco
2. Usuário altera campos, adiciona/remove telefones
3. Cada operação em telefone é **imediata** (INSERT/DELETE direto no banco)
4. "Salvar" grava alterações no contato e volta para Home

**Exclusão de contato (botão vermelho):**
- Coleta todos os dados (nome, idade, telefones) para o log
- Exclui via `ExcluirContato` (CASCADE remove telefones automaticamente)
- Volta para Home

### `Unit_Log.pas` — Sistema de Log

Grava no arquivo `log_alteracoes.txt` com formato:
```
[AAAA-MM-DD HH:MM:SS] [TIPO] Mensagem detalhada
```

**Tipos de evento (TTipoLog):**

| Enum | Texto no arquivo | Quando ocorre |
|------|-----------------|---------------|
| `tlCriacao` | `CRIACAO` | Novo contato salvo |
| `tlEdicao` | `EDICAO` | Contato existente alterado |
| `tlExclusao` | `EXCLUSAO` | Contato removido |
| `tlTelefoneAdd` | `TEL_ADD` | Telefone adicionado |
| `tlTelefoneRemove` | `TEL_REMOVE` | Telefone removido |

---

## Padrões Importantes do D2Bridge

| Conceito | Explicação |
|----------|-----------|
| **ExportD2Bridge** | Executa UMA VEZ por sessão. Monta o layout HTML. DataSources devem estar conectados ANTES de adicionar grids. |
| **InitControlsD2Bridge** | Configura controles após criação (colunas, ClickProc). |
| **DoPageLoaded (OnPageLoaded)** | Executa TODA VEZ que a página é exibida. Recarrega dados do banco. |
| **GetInstance / CreateInstance** | Cria ou obtém instância de outro form. Permite navegação multi-tela. |
| **ClickProc** | Lambda associado a um botão — executa código Delphi no servidor ao clicar. |
| **HTMLDIV** | Injeta HTML/CSS puro no layout (wrapper div com classes Bootstrap). |

---

## Banco de Dados

### Tabela: `contato`
| Coluna | Tipo | Observação |
|--------|------|-----------|
| ID | BIGINT | PK, AUTO_INCREMENT |
| NOME | VARCHAR(100) | NOT NULL |
| IDADE | SMALLINT | |

### Tabela: `telefone`
| Coluna | Tipo | Observação |
|--------|------|-----------|
| ID | BIGINT | PK, AUTO_INCREMENT |
| IDCONTATO | BIGINT | FK → contato.ID, ON DELETE CASCADE |
| NUMERO | VARCHAR(16) | NOT NULL |

O `ON DELETE CASCADE` garante que ao excluir um contato, todos os seus telefones são removidos automaticamente.

---

## Instalação e Execução

### Pré-requisitos

| Software | Versão | Link |
|----------|--------|------|
| Delphi | 12 (ou 10.0+) | [embarcadero.com](https://www.embarcadero.com/products/delphi/starter) |
| MySQL | 8.0+ | [mysql.com](https://dev.mysql.com/downloads/mysql/) |
| D2Bridge Framework | Beta | [github.com/d2bridge](https://github.com/d2bridge/d2bridgeframework) |

### 1. Instalar MySQL

```cmd
:: Baixar em https://dev.mysql.com/downloads/installer/
:: Ou via XAMPP: https://www.apachefriends.org/
```

### 2. Criar o Banco de Dados

```cmd
mysql -u root -p < database.sql
```

### 3. Configurar Conexão

Editar `bin/Config.ini`:

```ini
[Database]
Host=localhost
Port=3306
Database=agenda_contatos
User=root
Password=
```

### 4. Instalar D2Bridge Framework

```cmd
git clone https://github.com/d2bridge/d2bridgeframework.git
```

Executar `Beta\Wizard\InstallD2BridgeWizard.exe` com o Delphi fechado.

### 5. Compilar e Executar

1. Abrir `AgendaContatos.dpr` no Delphi
2. Target: **Windows 64-bit**
3. `Run → Run` (F9)
4. Clicar "Start" no form do servidor
5. Acessar: **http://localhost:8080**

### 6. DLLs necessárias (pasta `bin/`)

| Arquivo | Fonte |
|---------|-------|
| `libmysql.dll` (64-bit) | [MySQL Connector/C](https://dev.mysql.com/downloads/connector/c/) |
| `libcrypto-3-x64.dll` | Instalação do MySQL ou OpenSSL |
| `libssl-3-x64.dll` | Instalação do MySQL ou OpenSSL |

---

## Funcionalidades

| Funcionalidade | Status |
|---------------|--------|
| Cadastrar contato (nome, idade) | ✅ |
| Adicionar múltiplos telefones por contato | ✅ |
| Pesquisar por nome | ✅ |
| Pesquisar por número de telefone | ✅ |
| Editar contato selecionado | ✅ |
| Excluir contato selecionado | ✅ |
| Log completo de todas as operações | ✅ |
| Excluir telefone individual (lixeira) | ✅ |
| Interface responsiva (Bootstrap 5) | ✅ |

---

## Notas sobre o D2Bridge

- Converte forms Delphi (VCL) em páginas web automaticamente
- Gera HTML responsivo com Bootstrap 5
- Lógica de negócio executa no servidor (seguro — cliente não acessa SQL)
- Não é necessário escrever JavaScript
- Comunidade: [Discord](https://discord.gg/WvHaWP6h9t) | [Site](https://d2bridge.com.br)
