import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

import Generators


-- delayThread t = lift $ threadDelay (t * 100000)


-- get_message h = forever $ do
--                 delayThread (h_rate h)
--                 let m = Message { m_msg = "swap dis naw" , m_trace = "|", m_dest = (rem (h_rate h) 5)}
--                 yield m

-- print_message = forever $ do
--                 delayThread 1
--                 m <- await
--                 lift $ print $ m_msg m ++ "    " ++ m_trace m

-- sign_message n = forever $ do
--                 delayThread 1
--                 m <- await
--                 yield $ m { m_trace = (m_trace m) ++ "-> " ++ show n }


-- h_service h in_node out_node = runEffect $ recv_input in_node
--                                >-> send_output




-- r_service r in_nodes out_nodes = runEffect $ recv_input in_node
--                                 >-> send_output


-- get_output outs table = forever $ do
--                         m <- await
--                         let
--                             -- out = outputs !! (fst $ table !! (m_dest m))
--                             out = outputs !! 0
--                         lift $ ignore_m $ atomically $ send out m

-- ignore_m a = do x<-a
--                 return ()



main = do
    let
        writer = fst
        reader = snd
        num_humans = 5
        num_routers = 2
        num = num_humans + num_routers
        (humans, routers) = gen_agents num_humans num_routers


    let
        ids = [0..(num_humans + num_routers-1)]
        h_ids = take num_humans ids
        r_ids = drop num_humans ids

    pipes <- sequence $ map (\_ -> spawn unbounded) [0..(num-1)]

    let
        h_readers = [ reader $ pipes !! i | i <- h_ids ]
        h_writers = [ writer $ pipes !! (h_out (humans !! i)) | i <- h_ids ]

        ff = zip ids ((map (\a -> [a]) $ map h_out humans) ++ (map r_outs routers) )
        r_inputs = [ map fst $ filter (\(_,y) -> elem x y ) ff | x <- r_ids ]
        r_outputs = map r_outs routers


        r_readers = [ [ reader $ pipes !! j | j <- friends ] | friends <- r_inputs ]
        r_writers = [ [ writer $ pipes !! j | j <- friends ] | friends <- r_outputs ]


    print $ map h_id humans
    print $ map h_out humans
    print $ map r_id routers
    print $ map r_outs routers
    print $ r_inputs
    print $ r_outputs


    -- h_tasks <- sequence $ [async $ task | task <- zipWith3 h_service humans h_readers h_writers ]
    -- r_tasks <- sequence $ [async $ task | task <- zipWith3 r_service routers r_readers r_writers ]

    -- waitAny h_send_tasks
    return ()



