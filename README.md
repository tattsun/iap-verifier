# iap-verifier

A simple wrapper of In-App-Purchase receipt validate APIs.

## Usage

```
import Network.IAP.Verifier
main :: IO ()
main = do
  receipt <- readFile "./receipt"
  result <- verify defaultIAPSettings receipt
  case result of
    0 -> putStrLn "OK"
    _ -> putStrLn "Fail"
```
