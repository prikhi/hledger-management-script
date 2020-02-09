{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

import           Control.Monad                  ( foldM )
import           Data.Hashable                  ( Hashable )
import           Data.Maybe                     ( listToMaybe )
import           Development.Shake
import           Development.Shake.FilePath

import qualified Data.List                     as L

-- TODO: Re-organize so editable variables are all at the top, then main,
-- then build rules, then transformations on editable variables, then
-- utility functions.


-- Accounts with Statements to make Journals for

data ImportAccount
    = CapitalOneChecking
    | ETrade
    deriving (Show, Read, Eq, Enum, Bounded)

importAccountFolder :: ImportAccount -> String
importAccountFolder = \case
    CapitalOneChecking -> "capitalone/checking"
    ETrade             -> "etrade"

importCsvFolders :: [FilePath]
importCsvFolders = map (makeImportFolder "csv") [minBound .. maxBound]

importJournalFolders :: [FilePath]
importJournalFolders = map (makeImportFolder "journal") [minBound .. maxBound]

makeImportFolder :: String -> ImportAccount -> FilePath
makeImportFolder type_ account =
    "//import" </> importAccountFolder account </> type_ </> ("*" <.> type_)

importRules :: ImportAccount -> String
importRules = \case
    CapitalOneChecking -> "checking.rules"
    ETrade             -> "etrade.rules"

importJournalDependencies :: FilePath -> [String]
importJournalDependencies journalFile = either (: []) (const []) $ foldM
    (\_ account ->
        if ("//" ++ importAccountFolder account ++ "//*.journal")
                ?== journalFile
            then Left $ importRules account
            else Right ()
    )
    ()
    [minBound .. maxBound]


-- Opening / Closing Accounts

openingAccount :: String
openingAccount = "Equity:Opening Balances"

closingAccount :: String
closingAccount = "Equity:Closing Balances"

openCloseQuery :: String
openCloseQuery = "^(Assets|Liabilities):.*$"

-- Years to Track

currentYear :: Year
currentYear = 2020

firstYear :: Year
firstYear = 2020

newtype Year
    = Year Int
    deriving (Show, Read, Eq, Num, Enum, Bounded, Hashable)

allYears :: [Year]
allYears = [firstYear .. currentYear]


-- File Names

data ExportType
    = TransactionJournal
    | IncomeExpensesReport
    | BalanceSheetReport
    | CashFlowReport
    | AccountsReport
    | ClosingJournal
    | OpeningJournal
    deriving (Show, Read, Eq, Bounded, Enum)

yearJournal :: Year -> String
yearJournal (Year year) = show year ++ ".journal"

exportTypeFilenameSuffix :: ExportType -> FilePath
exportTypeFilenameSuffix type_ =
    let typeSuffix = case type_ of
            TransactionJournal   -> "all.journal"
            IncomeExpensesReport -> "income-expenses.txt"
            BalanceSheetReport   -> "balance-sheet.txt"
            CashFlowReport       -> "cash-flow.txt"
            AccountsReport       -> "accounts.txt"
            ClosingJournal       -> "closing.journal"
            OpeningJournal       -> "opening.journal"
    in  "-" ++ typeSuffix

allExportFiles :: [FilePath]
allExportFiles = concat
    [ [ "reports" </> show year ++ exportTypeFilenameSuffix type_
      | type_       <- [TransactionJournal .. AccountsReport]
      , (Year year) <- allYears
      ]
    , [ "opening" </> show year ++ exportTypeFilenameSuffix OpeningJournal
      | y@(Year year) <- allYears
      , y /= firstYear
      ]
    , [ "closing" </> show year ++ exportTypeFilenameSuffix ClosingJournal
      | y@(Year year) <- allYears
      , y /= currentYear
      ]
    ]


-- RULES


main :: IO ()
main = shakeArgs shakeOptions { shakeColor = True, shakeThreads = 0 } exportAll

exportAll :: Rules ()
exportAll = do
    want allExportFiles

    yearInputs <- newCache $ \year -> do
        let file = yearJournal year
        getIncludedFiles file

    TransactionJournal *%> processYear yearInputs ["print"]
    AccountsReport *%> processYear yearInputs ["accounts"]
    IncomeExpensesReport *%> processYear yearInputs ["is", "--flat"]
    BalanceSheetReport *%> processYear yearInputs ["bs"]
    CashFlowReport *%> processYear
        yearInputs
        ["cashflow", "not:desc:(opening balances)"]

    ClosingJournal *%> closeYear yearInputs
    OpeningJournal *%> openYear yearInputs

    importCsvFolders |%> cleanCsv
    importJournalFolders |%> csvToJournal
  where
    (*%>) :: ExportType -> (FilePath -> Action ()) -> Rules ()
    (*%>) type_ action_ = ("//*" ++ exportTypeFilenameSuffix type_) %> action_


-- ACTIONS


processYear :: (Year -> Action [FilePath]) -> [String] -> FilePath -> Action ()
processYear cachedYearIncludes args outputFile = do
    let year = Year . read . head $ split outputFile
    dependencies <- cachedYearIncludes year
    need dependencies
    (Stdout output) <-
        cmd ("hledger" :: String) $ "-f" : yearJournal year : args
    writeFileChanged outputFile output

openYear :: (Year -> Action [FilePath]) -> FilePath -> Action ()
openYear cachedYearIncludes outputFile = do
    let year         = Year . read . head $ split outputFile
        previousYear = year - 1
    need =<< cachedYearIncludes previousYear
    (Stdout output) <- cmd
        ("hledger" :: String)
        [ "-f"
        , yearJournal previousYear
        , "equity"
        , openCloseQuery
        , "-e"
        , (\(Year y) -> show y) year
        , "--opening"
        , "--open-to"
        , openingAccount
        ]
    writeFileChanged outputFile output

closeYear :: (Year -> Action [FilePath]) -> FilePath -> Action ()
closeYear cachedYearIncludes outputFile = do
    let year = read . head $ split outputFile
    processYear
        cachedYearIncludes
        [ "equity"
        , openCloseQuery
        , "-e"
        , (\(Year y) -> show $ y + 1) year
        , "-I"
        , "--closing"
        , "--close-to"
        , closingAccount
        ]
        outputFile

cleanCsv :: FilePath -> Action ()
cleanCsv outputFile = do
    let (csvDirectory, filename) = splitFileName outputFile
        sourceDirectory          = takeDirectory $ takeDirectory csvDirectory
        inputDirectory           = sourceDirectory </> "in"
    inputFiles <- getDirectoryFiles inputDirectory [filename -<.> "*"]
    let inputs = case inputFiles of
            [] -> error $ "No inputs for " ++ show filename
            _  -> map (inputDirectory </>) inputFiles
    let dependencies =
            map (sourceDirectory </>) $ importJournalDependencies outputFile
    need $ (sourceDirectory </> "in2csv.sh") : inputs ++ dependencies
    (Stdout output) <- cmd (Cwd sourceDirectory)
                           Shell
                           ("./in2csv.sh" :: String)
                           (map (makeRelative sourceDirectory) inputs)
    writeFileChanged outputFile output

csvToJournal :: FilePath -> Action ()
csvToJournal outputFile = do
    let (journalDirectory, filename) = splitFileName outputFile
        sourceDirectory = takeDirectory $ takeDirectory journalDirectory
        csvDirectory                 = sourceDirectory </> "csv"
        inputFile                    = csvDirectory </> (filename -<.> "csv")
        dependencies =
            map (sourceDirectory </>) $ importJournalDependencies outputFile
    need $ (sourceDirectory </> "csv2journal.sh") : inputFile : dependencies
    (Stdout output) <- cmd (Cwd sourceDirectory)
                           Shell
                           ("./csv2journal.sh" :: String)
                           [makeRelative sourceDirectory inputFile]
    writeFileChanged outputFile output


-- UTILS


-- Split a FilePath into the sub-components of it's filename.
--
-- E.g.: @"dir/a-b.ext" -> ["a", "-", "b", ".", "ext"]@
split :: FilePath -> [String]
split = takeWhile (/= "") . L.unfoldr (listToMaybe . lex) . takeFileName


-- Return a list of all @!include@ paths in the given file, prepended with
-- the passed file.
getIncludedFiles :: FilePath -> Action [FilePath]
getIncludedFiles file = do
    src <- liftIO $ readFile file
    let includes =
            [ f
            | x      <- lines src
            , Just f <-
                [L.stripPrefix "!include " x, L.stripPrefix "include " x]
            ]
    return $ file : includes
