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
                let m = Message { m_msg = "swap dis naw" , m_trace = "|", m_dest = (rem (h_rate h) 5)}
                yield m


print_message = forever $ do
                delayThread 1
                m <- await
                lift $ print $ m_msg m ++ "    " ++ m_trace m

sign_message id sym = forever $ do
                        delayThread 1
                        m <- await
                        yield $ m { m_trace = (m_trace m) ++ "->" ++ sym ++ show id }



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




r_service r input out_nodes = runEffect $ fromInput input
                                >-> sign_message (r_id r) symRouter
                                >-> r_route (r_table r) out_nodes


r_route table out_nodes = forever $ do
                            m <- await
                            let
                               out = get_node out_nodes $ table !! m_dest m
                            lift $ ignore_m $ atomically $ send out m

get_node nodes id = snd $ head $ filter (\(id, pipe) -> id==id) nodes



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
        -- ff = zip ids ((map (\a -> [a]) $ map h_out humans) ++ (map r_outs routers) )

        h_readers = [ reader $ pipes !! i | i <- h_ids ]
        h_writers = [ writer $ pipes !! (h_out h) | h <- humans ]

        r_readers = [ reader $ pipes !! i | i <- r_ids ]
        -- r_writers = [ [ writer $ pipes !! i | i <- outs ] | outs <- (map r_outs routers) ]
        r_writers = [ [ (i, writer $ pipes !! i) | i <- outs ] | outs <- (map r_outs routers) ]

    print $ ""
    print $ map h_id humans
    print $ map h_out humans
    print $ map r_id routers
    print $ map r_outs routers

    h_tasks <- sequence $ [async $ task | task <- zipWith3 h_service humans h_readers h_writers ]
    r_tasks <- sequence $ [async $ task | task <- zipWith3 r_service routers r_readers r_writers ]

    waitAny h_tasks
    -- return ()


