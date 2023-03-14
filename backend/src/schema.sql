CREATE TABLE IF NOT EXISTS node (
  idx INTEGER,
  hash TEXT,
  data TEXT,
  type TEXT,
  PRIMARY KEY (idx),
  UNIQUE (hash)
);

CREATE TABLE IF NOT EXISTS edge (
  source INTEGER,
  target INTEGER,
  group_num INTEGER,
  PRIMARY KEY (source, target),
  FOREIGN KEY (source) REFERENCES node(idx),
  FOREIGN KEY (target) REFERENCES node(idx)
);

CREATE TABLE IF NOT EXISTS path (
  destination INTEGER,
  steps BLOB,
  PRIMARY KEY (destination),
  FOREIGN KEY (destination) REFERENCES node(idx)
);

CREATE TABLE IF NOT EXISTS context (
  node_key TEXT,
  context_data TEXT,
  PRIMARY KEY (node_key)
);
