import Cardano.Prelude
import Cardano.Metadata.GoguenRegistry
import Cardano.Metadata.Types
import Data.Aeson
import Data.Aeson.Types
import Data.Text (unpack)
import qualified Options.Applicative as OA
import Prelude (String)
import qualified Data.ByteString.Lazy as B

data Arguments = Arguments
  { _ArgumentsFilename :: String
  , _ArgumentsRegistryEntry :: GoguenRegistryEntry Maybe
  }
  deriving Show

wellKnownOption :: WellKnownProperty p => (Text -> p) -> OA.Mod OA.OptionFields Text -> OA.Parser (WellKnown p)
wellKnownOption constr opts = asWellKnown <$> propertyValueOption where
  propertyValueOption = OA.option (OA.eitherReader (left unpack . propertyValueFromString)) opts
  asWellKnown pv@(PropertyValue txt _) = WellKnown pv $ constr txt

argumentParser :: OA.Parser Arguments
argumentParser = Arguments <$>
  OA.strArgument (OA.metavar "FILENAME") <*>
    (GoguenRegistryEntry
      <$> optional (Subject <$> OA.strOption (OA.long "subject" <> OA.short "s" <> OA.metavar "SUBJECT"))
      <*> optional (wellKnownOption Name (OA.long "name" <> OA.short "n" <> OA.metavar "NAME"))
      <*> pure Nothing -- XXX
      <*> pure Nothing
    )

combineRegistryEntries :: GoguenRegistryEntry Maybe -> GoguenRegistryEntry Maybe -> GoguenRegistryEntry Maybe
combineRegistryEntries new old = GoguenRegistryEntry
  { _goguenRegistryEntry_subject = _goguenRegistryEntry_subject new <|> _goguenRegistryEntry_subject old
  , _goguenRegistryEntry_name = _goguenRegistryEntry_name new <|> _goguenRegistryEntry_name old
  , _goguenRegistryEntry_description = _goguenRegistryEntry_description new <|> _goguenRegistryEntry_description old
  , _goguenRegistryEntry_preimage = _goguenRegistryEntry_preimage new <|> _goguenRegistryEntry_preimage old
  }

main :: IO ()
main = do
    Arguments filename newEntryInfo <- OA.execParser $ OA.info (argumentParser <**> OA.helper) mempty

    fileContents <- B.readFile filename
    WithOwnership owner record <- case decode fileContents of
        Just contents -> case parseEither parseRegistryEntry contents of
            Right res -> return res
            Left err -> do
                hPutStrLn stderr $ "Parse error: " <> err
                exitFailure
        Nothing -> do
            hPutStrLn stderr ("JSON parse error" :: String)
            exitFailure
    let newRecordWithOwnership = WithOwnership owner $ combineRegistryEntries newEntryInfo record

    writeFile filename $ show (serializeRegistryEntry newRecordWithOwnership) <> "\n"
    exitSuccess
