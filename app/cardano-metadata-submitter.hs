import Cardano.Prelude
import Cardano.Metadata.GoguenRegistry
import Data.Aeson
import Data.Aeson.Types
import qualified Options.Applicative as OA
import Prelude (String)
import qualified Data.ByteString.Lazy as B

data Arguments = Arguments
  { _ArgumentsFilename :: String
  }
  deriving (Show, Eq)

argumentParser :: OA.Parser Arguments
argumentParser = Arguments <$> OA.strArgument (OA.metavar "FILENAME")

main :: IO ()
main = do
    Arguments filename <- OA.execParser $ OA.info (argumentParser <**> OA.helper) mempty

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
