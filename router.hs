module Router where

import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

import Generators
import Message

import Debug.Trace as DB

r_service r input out_nodes = do
                                broadcast_task <- async $ r_broadcast_task r out_nodes
                                route_task <- async $ r_route_task r input out_nodes
                                wait route_task

r_route_task r input out_nodes = runEffect $ fromInput input
                                >-> sign_message (r_id r)
                                >-> r_route r out_nodes

r_broadcast_task r out_nodes = runEffect $ broadcast_message r out_nodes

broadcast_message r out_nodes = do
                                let m = Message { msg = Routing {n_table = r_table r, n_source = r_id r}
                                                , trc = (show (r_id r)) ++ "|"
                                                }
                                    outs =  get_nodes out_nodes (r_outs r) (r_table r)

                                    outs_ = DB.trace ("BB " ++ (show (length outs)) ++ " " ++ (show r) ++ show m ) outs

                                send_out outs_ m

send_out [] m = do return ()
send_out out m_ = do
                    let
                        m = DB.trace ("SEND " ++ show (length out) ++ " " ++ show m_ ) m_
                    send_message (head out) m
                    send_out (tail out) m


r_route r out_nodes = do
                        m <- await
                        delayThread 2
                        let
                           (m', outs_ids, table) = r_compute (msg m) r

                           r' = r { r_table = table }
                           m'' = Message { msg = m', trc = trc m}


                           outs = get_nodes out_nodes outs_ids table
                           outs_ = DB.trace ("RT " ++ (show (length outs)) ++ " " ++ (show r) ++ show m'' ) outs


                        send_out outs_ m''
                        r_route r' out_nodes


get_nodes nodes [] table = []
get_nodes nodes ids table = map ( (get_node nodes) . fst . (table !!) ) ids
get_node nodes id = snd $ head $ filter (\(id, pipe) -> id==id) nodes




r_compute :: Message' -> Router -> (Message', [Int], [(Int, Int)])
r_compute msg@Ping{} r = ( msg
                         , [m_dest msg]
                         , r_table r
                         )

r_compute msg@Routing {} r = ( Routing { n_table = new_table
                                       , n_source = r_id r
                                       }
                             , r_outs_if_changed
                             , new_table
                             )
                        where
                            new_table = improve_table (n_source msg) (n_table msg) (r_table r)
                            r_outs_if_changed = if new_table == (r_table r) then [] else r_outs r


improve_table s dists table = zipWith return_min_dist table table'
                            where
                                table' = map (\(n, d) -> (s, d+1)) dists

return_min_dist :: (Int, Int) -> (Int, Int) -> (Int, Int)
return_min_dist (n1,d1) (n2,d2)
                | d1 < d2   = (n1, d1)
                | otherwise = (n2, d2)


