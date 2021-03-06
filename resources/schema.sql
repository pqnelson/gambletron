CREATE TABLE team(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       lahman_id TEXT,
       league_id TEXT,
       year_id INT DEFAULT 0,
       franchise_id TEXT,
       division_id TEXT,
       rank INT DEFAULT 0,
       games_played INT DEFAULT 0,
       home_games INT DEFAULT 0,
       wins INT DEFAULT 0,
       losses INT DEFAULT 0,
       division_winner int,
       wild_card_winner int,
       league_champion int,
       world_series_winner int,
       runs INT DEFAULT 0,
       at_bats INT DEFAULT 0,
       hits INT DEFAULT 0,
       doubles INT DEFAULT 0,
       triples INT DEFAULT 0,
       homeruns INT DEFAULT 0,
       BB INT DEFAULT 0,
       SO INT DEFAULT 0,
       SB INT DEFAULT 0,
       CS INT DEFAULT 0,
       HBP INT DEFAULT 0,
       SF INT DEFAULT 0,
       RA INT DEFAULT 0,
       ER INT DEFAULT 0,
       ERA REAL,
       CG INT DEFAULT 0,
       SHO INT DEFAULT 0,
       SV INT DEFAULT 0,
       IPOuts INT DEFAULT 0,
       HA INT DEFAULT 0,
       HRA INT DEFAULT 0,
       BBA INT DEFAULT 0,
       SOA INT DEFAULT 0,
       E INT DEFAULT 0,
       DP INT DEFAULT 0,
       FP REAL,
       name TEXT NOT NULL,
       park_name TEXT,
       attendance INT DEFAULT 0,
       BPF INT DEFAULT 0,
       PPF INT DEFAULT 0,
       baseball_reference_id TEXT,
       old_lahman_id TEXT,
       retrosheet_id TEXT
       );

CREATE INDEX team_lahman_id_idx ON team (lahman_id, year_id);

CREATE TABLE player(
       id TEXT PRIMARY KEY,
       birthYear INT,
       birthMonth INT,
       birthDay INT,
       birthCountry TEXT,
       birthState TEXT,
       birthCity TEXT,
       deathYear INT,
       deathMonth INT,
       deathDay INT,
       deathCountry TEXT,
       deathState TEXT,
       deathCity TEXT,
       nameFirst TEXT,
       nameLast TEXT NOT NULL,
       nameGiven TEXT,
       weight INT,
       height INT,
       bats CHAR,
       throws CHAR,
       debut DATE,
       finalGame DATE,
       retro_id TEXT,
       bbref_id TEXT
       );

CREATE INDEX player_last_name_idx ON player (nameLast);

CREATE TABLE batting(
       id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
       player_id TEXT NOT NULL,
       year_id INTEGER NOT NULL,
       stint INTEGER NOT NULL,
       team_id TEXT NOT NULL,
       league_id TEXT NOT NULL,
       G INT DEFAULT 0,
       AB INT DEFAULT 0,
       R INT DEFAULT 0,
       H INT DEFAULT 0,
       "2B" INT DEFAULT 0,
       "3B" INT DEFAULT 0,
       HR INT DEFAULT 0,
       RBI INT DEFAULT 0,
       SB INT DEFAULT 0,
       CS INT DEFAULT 0,
       BB INT DEFAULT 0,
       SO INT DEFAULT 0,
       IBB INT DEFAULT 0,
       HBP INT DEFAULT 0,
       SH INT DEFAULT 0,
       SF INT DEFAULT 0,
       GIDP INT DEFAULT 0,
       FOREIGN KEY(player_id) REFERENCES player(id),
       FOREIGN KEY(team_id) REFERENCES team(id)
       );

--- Foreign keys do not automatically create indices, so we must make them
CREATE INDEX batting_player_idx ON batting (player_id, year_id);
CREATE INDEX batting_team_idx ON batting (team_id, year_id);

CREATE TABLE pitching(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       player_id TEXT NOT NULL,
       year_id INTEGER NOT NULL,
       stint INTEGER NOT NULL,
       team_id TEXT NOT NULL,
       league_id TEXT NOT NULL,
       W INT DEFAULT 0,
       L INT DEFAULT 0,
       G INT DEFAULT 0,
       GS INT DEFAULT 0,
       CG INT DEFAULT 0,
       SHO INT DEFAULT 0,
       SV INT DEFAULT 0,
       IPOuts INT DEFAULT 0,
       H INT DEFAULT 0,
       ER INT DEFAULT 0,
       HR INT DEFAULT 0,
       BB INT DEFAULT 0,
       SO INT DEFAULT 0,
       BAOpp REAL,
       ERA REAL,
       IBB INT DEFAULT 0,
       WP INT DEFAULT 0,
       HBP INT DEFAULT 0,
       BK INT DEFAULT 0,
       BFP INT DEFAULT 0,
       GF INT DEFAULT 0,
       R INT DEFAULT 0,
       SH INT DEFAULT 0,
       SF INT DEFAULT 0,
       GIDP INT DEFAULT 0,
       FOREIGN KEY(player_id) REFERENCES player(id),
       FOREIGN KEY(team_id) REFERENCES team(id)
       );
       
CREATE INDEX pitching_player_idx ON pitching (player_id, year_id);
CREATE INDEX pitching_team_idx ON pitching (team_id, year_id);

CREATE TABLE fielding(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       player_id TEXT NOT NULL,
       year_id INTEGER NOT NULL,
       stint INTEGER,
       team_id TEXT NOT NULL,
       league_id TEXT,
       Pos TEXT,
       G INT DEFAULT 0,
       GS INT DEFAULT 0,
       InnOuts INT DEFAULT 0,
       PO INT DEFAULT 0,
       A INT DEFAULT 0,
       E INT DEFAULT 0,
       DP INT DEFAULT 0,
       PB INT DEFAULT 0,
       WP INT DEFAULT 0,
       SB INT DEFAULT 0,
       CS INT DEFAULT 0,
       ZR INT DEFAULT 0,
       FOREIGN KEY(player_id) REFERENCES player(id),
       FOREIGN KEY(team_id) REFERENCES team(id)
       );

CREATE INDEX fielding_player_idx ON fielding (player_id, year_id);
CREATE INDEX fielding_team_idx ON fielding (team_id, year_id);

