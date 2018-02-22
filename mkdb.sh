#!/usr/bin/env bash

sqlite3 links.db<<EOF
CREATE TABLE links (id ROWID, name TEXT UNIQUE, url TEXT UNIQUE, hits INT DEFAULT 0, created_at TEXT);
CREATE INDEX links_hits on links(hits);
EOF
