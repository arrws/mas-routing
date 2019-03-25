module Message where

import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

import Generators


delayThread t = lift $ threadDelay (t * 100000)

gen_message h = forever $ do
                delayThread (h_rate h)
                let m = Message { msg = Ping { m_msg = "ping" , m_dest = (rem (h_rate h) 5)}
                                , trace = "|"
                                }
                yield m

print_message :: Proxy () Message y y' IO b
print_message = forever $ do
                delayThread 1
                m <- await
                -- lift $ print $ trace m
                lift $ print $ show m

sign_message id = forever $ do
                        delayThread 1
                        m <- await
                        let m' = Message { msg = msg m, trace = (trace m) ++ "->" ++ show id }
                        yield $ m'

send_message out m = lift $ ignore_m $ atomically $ send out m


ignore_m a = do x<-a
                return ()


