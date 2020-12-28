module Dom.Cred (module Dom.Cred) where

import Data.ByteString
import Data.X509                             qualified as X509
import Data.X509.CertificateStore            qualified as X509
import Network.TLS                           qualified as TLS

import Network.WebSockets.Connection.Options qualified as WS


data Cred
  = Cred
    { cCred     :: !TLS.Credentials
    , cIdentity :: !X509.CertificateStore
    }

data CredSpec
  = CredFile
    { cfCertFile :: !FilePath
    , cfKeyFile  :: !FilePath
    }
  | CredMemory
    { cmCert     :: !ByteString
    , cmKey      :: !ByteString
    }

loadCred :: CredSpec -> IO (Either String Cred)
loadCred = fmap (fmap toCred) . \case
  CredFile   cert key -> TLS.credentialLoadX509                  cert key
  CredMemory cert key -> pure $ TLS.credentialLoadX509FromMemory cert key
 where
   toCred :: TLS.Credential -> Cred
   toCred cred@(X509.CertificateChain chain, _) =
     Cred
     { cCred     = TLS.Credentials [cred]
     , cIdentity = X509.makeCertificateStore chain
     }

toWSCertSettings :: CredSpec -> WS.CertSettings
toWSCertSettings = \case
  CredFile   cert key -> WS.CertFromFile   cert [] key
  CredMemory cert key -> WS.CertFromMemory cert [] key
