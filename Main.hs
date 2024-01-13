import System.Directory (
    listDirectory, doesFileExist, doesDirectoryExist,
    getModificationTime, renameFile )
import System.FilePath ( takeDirectory, takeBaseName, takeExtension )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import System.Environment ( getArgs )
import Data.Foldable ( traverse_ )
import System.Exit ( exitFailure, exitSuccess )

allFiles ::  FilePath -> IO [FilePath]
allFiles path = do
    paths <- filter (\p -> p /= "." && p /= "..") . ((\p -> path ++ "/" ++ p) <$>) <$> listDirectory path
    filesExistences <- traverse doesFileExist paths
    foldersExistences <- traverse doesDirectoryExist paths

    let files = map fst $ filter snd $ zip paths filesExistences
    let folders = map fst $ filter snd $ zip paths foldersExistences

    innerFoldersFiles <- traverse allFiles folders

    return $ files ++ concat innerFoldersFiles

prependModificationTime :: FilePath -> IO ()
prependModificationTime path = do
    let directory = takeDirectory path
    let baseName = takeBaseName path
    let extension = takeExtension path

    modificationTime <- getModificationTime path
    
    let modificationTimeString = formatTime defaultTimeLocale "%Y%m%d-%H%M%S-" modificationTime
    let newPath = directory ++ "/" ++ modificationTimeString ++ baseName ++ extension

    renameFile path newPath
    putStrLn $ "Renaming: " ++ path

main :: IO ()
main = do
    args <- getArgs

    if length args /= 1 then do
        putStrLn "Missing folder path."
        exitFailure

    else do
        let [path] = args
        let slashPath = (\c -> if c == '\\' then '/' else c) <$> path

        exists <- doesDirectoryExist slashPath

        if not exists then do
            putStrLn "Folder does not exist."
            exitFailure

        else do
            putStrLn slashPath
            allFiles slashPath >>= traverse_ prependModificationTime
            exitSuccess
