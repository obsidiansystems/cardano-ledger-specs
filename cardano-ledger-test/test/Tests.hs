
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Data.Proxy
import Test.Cardano.Ledger.Elaborators.Shelley ()
import Test.Cardano.Ledger.Elaborators.Alonzo ()
import Test.Cardano.Ledger.ModelChain (newTestFw)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Tasty (TestTree, testGroup, defaultMain)

tests :: TestTree
tests =
  testGroup "new-unit-tests"
    [ newTestFw (Proxy :: Proxy (ShelleyEra C_Crypto))
    , newTestFw (Proxy :: Proxy (AlonzoEra C_Crypto))
    ]

main :: IO ()
main = defaultMain tests
