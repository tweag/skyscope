CREATE TABLE IF NOT EXISTS node (
  idx INTEGER,
  hash TEXT,
  data TEXT,
  type TEXT,
  PRIMARY KEY (idx) ON CONFLICT IGNORE,
  UNIQUE (hash) ON CONFLICT IGNORE
);

CREATE TABLE IF NOT EXISTS edge (
  source INTEGER,
  target INTEGER,
  group_num INTEGER,
  PRIMARY KEY (source, target) ON CONFLICT IGNORE,
  FOREIGN KEY (source) REFERENCES node(idx),
  FOREIGN KEY (target) REFERENCES node(idx)
);

CREATE INDEX IF NOT EXISTS target_index ON edge (target);

CREATE TABLE IF NOT EXISTS path (
  destination INTEGER,
  steps BLOB,
  PRIMARY KEY (destination),
  FOREIGN KEY (destination) REFERENCES node(idx)
);

CREATE TABLE IF NOT EXISTS context (
  context_key TEXT,
  context_data TEXT,
  PRIMARY KEY (context_key) ON CONFLICT REPLACE
);
