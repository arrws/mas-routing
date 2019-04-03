import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

import Generators
import Human
import Router
import Message

main = do
    let
        writer = fst
        reader = snd

        num_humans = 3
        num_routers = 8
        num = num_humans + num_routers

        ids = [0..(num_humans + num_routers-1)]
        h_ids = take num_humans ids
        r_ids = drop num_humans ids

        (h_links, r_links) = gen_links h_ids r_ids
        humans = gen_humans h_ids h_links
        routers = gen_routers r_ids r_links



    pipes <- sequence $ map (\_ -> spawn unbounded) [0..(num-1)]

    let
        h_readers = [ reader $ pipes !! i | i <- h_ids ]
        h_writers = [ writer $ pipes !! (h_out h) | h <- humans ]

        r_readers = [ reader $ pipes !! i | i <- r_ids ]
        r_writers = [ [ (i, writer $ pipes !! i) | i <- outs ] | outs <- (map r_outs routers) ]

    print $ map h_id humans
    print $ map h_out humans
    print $ map r_id routers
    print $ map r_outs routers
    putStr $ foldr (\x b -> b ++ "\n" ++ show x) "" humans
    putStr $ foldr (\x b -> b ++ "\n" ++ show x ++ "\n" ++ stringify_table (r_table x)) "" routers

    h_tasks <- sequence $ [async $ task | task <- zipWith3 h_service humans h_readers h_writers ]
    r_tasks <- sequence $ [async $ task | task <- zipWith3 r_service routers r_readers r_writers ]
    waitAny h_tasks

    return ()


