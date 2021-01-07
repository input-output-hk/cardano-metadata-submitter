import Cardano.Prelude
import Cardano.Metadata.GoguenRegistry
import Data.Aeson
import Data.Aeson.Types
import Prelude (String)
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    (filename:_) <- getArgs
    fileContents <- B.readFile filename
    case decode fileContents of
        Just contents -> case parseEither parseRegistryEntry contents of
            Right res -> do
                print res
                exitSuccess
            Left err -> do
                hPutStrLn stderr $ "Parse error: " <> err
                exitFailure
        Nothing -> do
            hPutStrLn stderr ("JSON parse error" :: String)
            exitFailure
