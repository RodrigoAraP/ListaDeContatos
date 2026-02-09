-- ==========================================================================
--  Agenda de Contatos - Script de Criacao do Banco de Dados (MySQL 8+)
--
--  Uso: mysql -u root -p < database.sql
-- ==========================================================================

CREATE DATABASE IF NOT EXISTS agenda_contatos
  CHARACTER SET utf8mb4
  COLLATE utf8mb4_general_ci;

USE agenda_contatos;

-- --------------------------------------------------------------------------
--  Tabela: Contato
-- --------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS contato (
  ID         BIGINT        NOT NULL AUTO_INCREMENT,
  NOME       VARCHAR(100)  NOT NULL,
  IDADE      SMALLINT      NULL,
  PRIMARY KEY (ID)
) ENGINE=InnoDB;

-- --------------------------------------------------------------------------
--  Tabela: Telefone
-- --------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS telefone (
  ID         BIGINT        NOT NULL AUTO_INCREMENT,
  IDCONTATO  BIGINT        NOT NULL,
  NUMERO     VARCHAR(16)   NOT NULL,
  PRIMARY KEY (ID),
  CONSTRAINT FK_Telefone_Contato
    FOREIGN KEY (IDCONTATO) REFERENCES contato(ID)
    ON DELETE CASCADE
    ON UPDATE CASCADE
) ENGINE=InnoDB;

-- --------------------------------------------------------------------------
--  Indice para acelerar pesquisa por numero de telefone
-- --------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS IDX_Telefone_Numero ON telefone(NUMERO);

-- --------------------------------------------------------------------------
--  Dados de exemplo (opcional - remover em producao)
-- --------------------------------------------------------------------------
INSERT INTO contato (NOME, IDADE) VALUES ('Maria Silva', 30);
INSERT INTO contato (NOME, IDADE) VALUES ('João Santos', 25);
INSERT INTO contato (NOME, IDADE) VALUES ('Ana Oliveira', 42);

INSERT INTO telefone (IDCONTATO, NUMERO) VALUES (1, '(11) 99999-0001');
INSERT INTO telefone (IDCONTATO, NUMERO) VALUES (1, '(11) 3333-0001');
INSERT INTO telefone (IDCONTATO, NUMERO) VALUES (2, '(21) 98888-0002');
INSERT INTO telefone (IDCONTATO, NUMERO) VALUES (3, '(31) 97777-0003');
INSERT INTO telefone (IDCONTATO, NUMERO) VALUES (3, '(31) 3555-0003');
