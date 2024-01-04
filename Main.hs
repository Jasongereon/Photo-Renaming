import System.Directory
import System.FilePath
import Data.Time.Format
import Data.Time.Clock

allFiles ::  FilePath -> IO [FilePath]
allFiles path = do
    paths <- filter (\p -> p /= "." && p /= "..") . ((\p -> path ++ "/" ++ p) <$>) <$> listDirectory path
    filesExistances <- traverse doesFileExist paths
    let files = map fst $ filter snd $ zip paths filesExistances
    foldersExistances <- traverse doesDirectoryExist paths
    let folders = map fst $ filter snd $ zip paths foldersExistances

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

imagePath :: String
imagePath = "C:/Users/Jasongereon/Desktop/Y2"

main :: IO [()]
main = allFiles imagePath >>= traverse prependModificationTime
