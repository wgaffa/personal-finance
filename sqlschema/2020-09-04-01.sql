CREATE TABLE "TransactionTypes" (
    "id" INTEGER UNIQUE,
    "name" TEXT NOT NULL,
    PRIMARY KEY("id")
);

INSERT INTO "TransactionTypes" (name) VALUES ("Debit"), ("Credit");

CREATE TABLE "Transactions" (
    "id" INTEGER UNIQUE,
    "date" Date NOT NULL,
    "account_id" INTEGER NOT NULL,
    "type_id" INTEGER NOT NULL,
    "amount" INTEGER NOT NULL,
    PRIMARY KEY("id"),
    FOREIGN KEY("type_id") REFERENCES TransactionTypes ("id"),
    FOREIGN KEY("account_id") REFERENCES Accounts ("id")
);