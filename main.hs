import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

import Generators


delayThread t = lift $ threadDelay (t * 100000)

get_message h = forever $ do
                delayThread (h_rate h)
                let m = Ping { m_msg = "swap dis naw" , m_trace = "|", m_dest = (rem (h_rate h) 5)}
                yield m

print_message = forever $ do
                delayThread 1
                m <- await
                lift $ print $ m_msg m ++ "    " ++ m_trace m

sign_message id sym = forever $ do
                        delayThread 1
                        m <- await
                        yield $ m { m_trace = (m_trace m) ++ "->" ++ sym ++ show id }

send_message out m = lift $ ignore_m $ atomically $ send out m




h_service h input out_node = do
                                send_task <- async $ h_send_msg h out_node
                                recv_task <- async $ h_recv_msg h input
                                wait send_task

h_send_msg h out_node = runEffect $ get_message h
                        >-> sign_message (h_id h) symHuman
                        >-> toOutput out_node

h_recv_msg h input = runEffect $ fromInput input
                        >-> sign_message (h_id h) symHuman
                        >-> print_message




r_service r input out_nodes = runEffect $ broadcast_message r out_nodes
                                >-> fromInput input
                                >-> sign_message (r_id r) symRouter
                                >-> r_route r out_nodes


broadcast_message r out_nodes = do
                let m = Routing {n_table = r_table r, n_source = r_id r}
                    outs =  get_nodes out_nodes (r_outs r) (r_table r)
                send_out outs m


r_route r out_nodes = forever $ do
                            m <- await
                            let
                               (m', outs_ids, table) = r_compute m r
                               r = r { r_table = table }
                               outs =  get_nodes out_nodes outs_ids (r_table r)
                            send_out outs m'

get_nodes nodes ids table = map ( (get_node nodes) . fst . (table !!) ) ids
get_node nodes id = snd $ head $ filter (\(id, pipe) -> id==id) nodes


send_out [] m = do return ()
send_out out m = do
                    send_message (head out) m
                    send_out (init out) m


-- r_compute :: Message -> [(Int, Int)] -> (Message, (Int, Int), [(Int, Int)])
r_compute Ping { m_msg = msg
               , m_trace = trace
               , m_dest = dest
                }
                r = (Ping { m_msg = msg
                              , m_trace = trace
                              , m_dest = dest
                              }
                        , [dest]
                        , r_table r)
r_compute Routing { n_table = table
                  , n_source = source
                  }
                r = (Routing { n_table = new_table
                               , n_source = r_id r
                             }
                        , r_outs r
                        , new_table)
                 where
                     new_table = improve_table source table (r_table r)


improve_table source t table = zipWith return_min_dist t' table
                                where
                                    t' = map (\(n, d) -> (source, d+1)) t

return_min_dist :: (Int, Int) -> (Int, Int) -> (Int, Int)
return_min_dist (n1,d1) (n2,d2)
                                | d1 < d2   = (n1, d1)
                                | otherwise   = (n2, d2)



ignore_m a = do x<-a
                return ()


main = do
    let
        writer = fst
        reader = snd
        num_humans = 3
        num_routers = 2
        num = num_humans + num_routers
        (humans, routers) = gen_agents num_humans num_routers

        ids = [0..(num_humans + num_routers-1)]
        h_ids = take num_humans ids
        r_ids = drop num_humans ids


    pipes <- sequence $ map (\_ -> spawn unbounded) [0..(num-1)]

    let
        h_readers = [ reader $ pipes !! i | i <- h_ids ]
        h_writers = [ writer $ pipes !! (h_out h) | h <- humans ]

        r_readers = [ reader $ pipes !! i | i <- r_ids ]
        r_writers = [ [ (i, writer $ pipes !! i) | i <- outs ] | outs <- (map r_outs routers) ]

    print $ ""
    print $ map h_id humans
    print $ map h_out humans
    print $ map r_id routers
    print $ map r_outs routers

    h_tasks <- sequence $ [async $ task | task <- zipWith3 h_service humans h_readers h_writers ]
    r_tasks <- sequence $ [async $ task | task <- zipWith3 r_service routers r_readers r_writers ]

    waitAny h_tasks


