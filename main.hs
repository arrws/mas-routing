import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

import Generators
import Sender
import Router
import Message

main = do
    let
        writer = fst
        reader = snd

        num_senders = 3
        num_routers = 11
        num = num_senders + num_routers

        ids = [0..(num_senders + num_routers - 1)]
        s_ids = take num_senders ids
        r_ids = drop num_senders ids

        --- generate random sender and router agents
        (s_links, r_links) = gen_links s_ids r_ids
        senders = gen_senders s_ids s_links
        routers = gen_routers r_ids r_links

    --- spawn an async thread for each agent
    pipes <- replicateM num $ spawn unbounded

    let
        --- compute the corresponding comunication pipes between agents
        s_readers = [ reader $ pipes !! i | i <- s_ids ]
        s_writers = [ writer $ pipes !! (s_out h) | h <- senders ]
        r_readers = [ reader $ pipes !! i | i <- r_ids ]
        r_writers = [ [ (i, writer $ pipes !! i) | i <- outs ] | outs <- (map r_outs routers) ]

    print $ map s_id senders
    print $ map s_out senders
    print $ map r_id routers
    print $ map r_outs routers
    -- print "-----"
    -- print $ length pipes
    -- print $ length s_readers
    -- print $ length s_writers
    -- print $ length r_readers
    -- print $ length r_writers
    -- print $ [ i | i <- s_ids ]
    -- print $ [ (s_out h) | h <- senders ]
    -- print $ [ i | i <- r_ids ]
    -- print $ [ [ (i, i) | i <- outs ] | outs <- (map r_outs routers) ]

    --- begin simulation
    s_tasks <- sequence $ [async $ task | task <- zipWith3 s_service senders s_readers s_writers ]
    r_tasks <- sequence $ [async $ task | task <- zipWith3 r_service routers r_readers r_writers ]
    waitAny r_tasks

    return ()


