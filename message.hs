module Message where

import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Monad.Writer
-- import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Identity

import Generators

type WMessage = Writer [Int] Message

evalWriter :: WMessage -> Message
evalWriter m = fst $ runWriter m


delayThread t = lift $ threadDelay (t * 100000)

gen_message :: Human -> Proxy a b () WMessage IO c
gen_message h = forever $ do
                delayThread (h_rate h)
                let msg = Ping { m_msg = "ping" , m_dest = rem (h_id h +1) 2 }
                    m = sign_message msg (h_id h) msg
                yield m


print_message h = forever $ do
                delayThread 1
                m <- await
                let msg = evalWriter m
                    trace = execWriter $ m >>= sign_message msg (h_id h)
                lift $ putStr $ to_str msg trace



to_str :: Message -> [Int] -> [Char]
-- to_str _ = ""
to_str Ping {m_msg=msg, m_dest=dest} trc
            = "PING: " ++ msg ++ " to " ++ show(dest) ++ " trc: " ++ stringify_trace trc ++ "\n\n"
to_str Routing {n_table=table, n_source=source} trc
            = "ROUTING: from " ++ show(source) ++ " trc: " ++ stringify_trace trc ++ "\n" ++ stringify_table table ++ "\n\n"

stringify_trace = show

stringify_table t = l0 ++ l1 ++ l2
                where
                    tabify f = foldr (\x l -> l ++ "\t" ++ show ( f x)) " "
                    l0 ="\tdest_node:" ++ (tabify id [0..length t -1]) ++ "\n"
                    l1 ="\troute_to:" ++ (tabify fst t) ++ "\n"
                    l2 ="\tdistance:" ++  (tabify snd t) ++ "\n"


sign_message :: Message -> Int -> Message -> WMessage
sign_message m id _ = do
                    tell [id]
                    return m

sign :: Int -> Message -> WMessage
sign id m = do
                tell [id]
                return m

send_message :: (MonadTrans t) => Output WMessage -> WMessage -> t IO ()
send_message out m = lift $ void $ atomically $ send out m


