PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS "AccountElement" (
	"id"	INTEGER NOT NULL,
	"name"	TEXT NOT NULL,
	PRIMARY KEY("id")
);

CREATE TABLE "Accounts" (
	"id"	INTEGER UNIQUE,
	"name"	TEXT NOT NULL,
	"element_id"	INTEGER NOT NULL,
	PRIMARY KEY("id"),
	FOREIGN KEY("element_id") REFERENCES AccountElement ("id")
);

INSERT INTO "AccountElement" (name) VALUES ("Asset"), ("Liability"), ("Equity"), ("Income"), ("Expenses");