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
                lift $ putStr $ to_str m



to_str :: Message -> [Char]
to_str Message { msg = Ping {m_msg=msg, m_dest=dest}
               , trace = trace
               } = "PING: " ++ msg ++ " to " ++ show(dest) ++ " trace: " ++ trace ++ "\n\n"
-- to_str _ = ""
to_str Message { msg = Routing {n_table=table, n_source=source}
               , trace = trace
               } = "ROUTING: from " ++ show(source) ++ " trace: " ++ trace ++ "\n" ++ str_table ++ "\n\n"
                where
                    l0 ="\tdest_node:" ++ (tabify id [0..length table -1]) ++ "\n"
                    l1 ="\troute_to:" ++ (tabify fst table) ++ "\n"
                    l2 ="\tdistance:" ++  (tabify snd table) ++ "\n"
                    tabify f = foldr (\x l -> l ++ "\t" ++ show ( f x)) " "
                    str_table = l0 ++ l1 ++ l2


sign_message id = forever $ do
                        delayThread 1
                        m <- await
                        let m' = Message { msg = msg m, trace = (trace m) ++ "->" ++ show id }
                        yield $ m'

send_message out m = lift $ ignore_m $ atomically $ send out m


ignore_m a = do x<-a
                return ()


