module Yggdrasil.Test.Yggdrasil where

import Relude
import Test.Hspec
import Yggdrasil

yggdrasilSpec :: SpecWith ()
yggdrasilSpec = describe "yggdrasil" $ do
  it "can parse sql DDL statements for SQLite" $ do
    f <- readFileBS "./resources/test/migration-files/1-some-valid-sql.sql"
    case decodeUtf8' f of
      Left e -> print e >> error (show e)
      Right t -> do
        let parsed = parseTextToSqlStatements t
        length parsed `shouldBe` 3
  it "can explode migration filename properly" $ do
    explodeMigrationPath "0-init.sql" `shouldBe` Just ("0", "0-init.sql")
    explodeMigrationPath "999999-init.sql" `shouldBe` Just ("999999", "999999-init.sql")
    explodeMigrationPath "1-init🔗❤️👍.sql" `shouldBe` Just ("1", "1-init🔗❤️👍.sql")
    explodeMigrationPath "23-init with spaces and weird symbols$.sql" `shouldBe` Just ("23", "23-init with spaces and weird symbols$.sql")
  it "gets files in the right order" $ do
    xs <- getSortedMigrationFiles defaultYggdrasil {migrationsDirectoryPath = "./resources/test/migration-files/"}
    xs `shouldBe` [(0, "0-empty.sql"), (1, "1-some-valid-sql.sql"), (2, "2-more-valid-sql-$$.sql")]
