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


r_service r input out_nodes = do
                                route_task <- async $ r_route_task r input out_nodes
                                broadcast_task <- async $ r_broadcast_task r out_nodes
                                wait route_task

r_route_task r input out_nodes = runEffect $ fromInput input
                                >-> sign_message (r_id r)
                                >-> r_route r out_nodes

-- r_service r input out_nodes = runEffect $ broadcast_message r out_nodes
                                -- >-> fromInput input
                                -- -- >-> sign_message (r_id r)
                                -- >-> r_route r out_nodes

r_broadcast_task r out_nodes = runEffect $ broadcast_message r out_nodes

broadcast_message r out_nodes = do
                let m = Message { msg = Routing {n_table = r_table r, n_source = r_id r}
                                , trace = "|"
                                }
                    outs =  get_nodes out_nodes (r_outs r) (r_table r)
                send_out outs m


r_route r out_nodes = do
                        m <- await
                        let
                           (m', outs_ids, table) = r_compute (msg m) r
                           r' = Router { r_id = r_id r
                                      , r_outs = r_outs r
                                      , r_table = table }

                           m'' = Message { msg = m', trace = trace m}

                           outs =  get_nodes out_nodes outs_ids table

                        send_out outs m''

                        r_route r' out_nodes


get_nodes nodes ids table = map ( (get_node nodes) . fst . (table !!) ) ids
get_node nodes id = snd $ head $ filter (\(id, pipe) -> id==id) nodes


send_out [] m = do return ()
send_out out m = do
                    send_message (head out) m
                    send_out (tail out) m


-- r_compute :: Message' -> Router -> (Message', [Int], [(Int, Int)])

r_compute Ping  { m_msg = msg
                , m_dest = dest
                }
                r = (Ping   { m_msg = msg
                            , m_dest = dest
                            }
                    , [dest]
                    , r_table r
                    )

r_compute Routing   { n_table = table
                    , n_source = source
                    }
                    r = (Routing    { n_table = new_table
                                    , n_source = r_id r
                                    }
                        , r_outs_if_changed
                        , new_table
                        )
                    where
                        new_table = improve_table source table (r_table r)
                        r_outs_if_changed = if new_table == table then [] else r_outs r


improve_table source t table = zipWith return_min_dist t' table
                                where
                                    t' = map (\(n, d) -> (source, d+1)) t

return_min_dist :: (Int, Int) -> (Int, Int) -> (Int, Int)
return_min_dist (n1,d1) (n2,d2)
                                | d1 < d2   = (n1, d1)
                                | otherwise = (n2, d2)



ignore_m a = do x<-a
                return ()

