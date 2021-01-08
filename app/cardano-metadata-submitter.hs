import Cardano.Prelude
import Cardano.Metadata.GoguenRegistry
import Data.Aeson
import Data.Aeson.Types
import Prelude (String)
import qualified Data.ByteString.Lazy as B
import System.Console.ParseArgs

data ArgumentIndex
    = ArgumentIndexFilename
    deriving (Show, Ord, Eq)

argInfo :: [Arg ArgumentIndex]
argInfo =
    [ Arg ArgumentIndexFilename Nothing Nothing (argDataRequired "filename" ArgtypeString) "filename"
    ]

main :: IO ()
main = do
    let argStrictness = ArgsParseControl ArgsComplete ArgsSoftDash

    args <- parseArgsIO argStrictness argInfo
    filename <- case getArgString args ArgumentIndexFilename of
        Just fname -> return fname
        Nothing -> do
            hPutStrLn stderr ("No filename provided" :: String)
            exitFailure

    fileContents <- B.readFile filename
    record <- case decode fileContents of
        Just contents -> case parseEither parseRegistryEntry contents of
            Right res -> return res
            Left err -> do
                hPutStrLn stderr $ "Parse error: " <> err
                exitFailure
        Nothing -> do
            hPutStrLn stderr ("JSON parse error" :: String)
            exitFailure

    writeFile filename $ show (serializeRegistryEntry record) <> "\n"
    exitSuccess
