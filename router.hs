module Router where

import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Monad.Writer

import Generators
import Message

import Debug.Trace as DB

r_service r input out_nodes = do
                                broadcast_task <- async $ r_broadcast_task r out_nodes
                                route_task <- async $ r_route_task r input out_nodes
                                wait route_task

r_route_task r input out_nodes = runEffect $ fromInput input
                                >-> r_route r out_nodes

r_broadcast_task r out_nodes = runEffect $ broadcast_message r out_nodes


get_nodes nodes [] table = []
get_nodes nodes ids table = map ( (get_node nodes) . fst . (table !!) ) ids
get_node nodes id' = snd $ head $ filter (\(id, pipe) -> id==id') nodes

send_out :: (Monad (t IO), MonadTrans t) => [Output WMessage] -> WMessage -> t IO ()
send_out outs m = mapM_ (flip send_message m) outs


broadcast_message :: (Monad (t IO), MonadTrans t) => Router -> [(Int, Output WMessage)] -> t IO ()
broadcast_message r out_nodes = do
                                delayThread 5
                                let msg = Routing {n_table = r_table r, n_source = r_id r}
                                    m = sign_message msg (r_id r) msg
                                    -- outs = DB.trace ("broadcast " ++ show (evalWriter m) ++ "    ") get_nodes out_nodes (r_outs r) (r_table r)
                                    outs = get_nodes out_nodes (r_outs r) (r_table r)
                                send_out outs m


r_route :: Router -> [(Int, Output WMessage)] -> Proxy () WMessage y' y IO b
r_route r out_nodes = do
                        delayThread 1
                        m <- await
                        let (r', msg, next_ids) = process r (evalWriter m)
                            m' = m >>= (sign_message msg (r_id r))
                            next_nodes = get_nodes out_nodes next_ids (r_table r')
                        send_out next_nodes m'
                        r_route r' out_nodes


process :: Router -> Message -> (Router, Message, [Int])
process r msg@Ping{} = (r, msg, [m_dest msg])
process r msg@Routing{} = (r', msg', ids)
                            where
                                new_table = improve_table (n_source msg) (n_table msg) (r_table r)
                                ids = if new_table == (r_table r) then [] else r_outs r
                                r' = r {r_table = new_table}
                                msg' = Routing {n_table = new_table, n_source = r_id r }


improve_table s dists table = zipWith return_min_dist table table'
                            where
                                table' = map (\(n, d) -> (s, d+1)) dists

return_min_dist :: (Int, Int) -> (Int, Int) -> (Int, Int)
return_min_dist (n1,d1) (n2,d2)
                | d1 > d2   = (n2, d2)
                | otherwise = (n1, d1)


