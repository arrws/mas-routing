import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)


data Human = Human { hname :: Int
                   , houts :: Int
                   , gen_rate :: Int
                   }

data Router = Router { rname :: Int
                     , routs :: [Int]
                     , table :: [Int]
                     }

data Message = Message { mname :: Int
                       , len :: Int
                       , msg :: [Char]
                       , history :: [Char]
                       , dest :: Int
                       }

class Name x where
    name :: x -> Int
instance Name Human where name = hname
instance Name Router where name = rname

-- class Outs x where
--     outs :: x -> [Int]
-- instance Outs Human where outs = houts
-- instance Outs Router where outs = routs


delayThread t = lift $ threadDelay (t * 100000)

getMessage h = forever $ do
            delayThread (gen_rate h)
            let m = Message { msg = "fuckthis" , history = "|", dest = (rem (gen_rate h) 5)}
            yield m

printMessage = forever $ do
                delayThread 1
                m <- await
                lift $ print $ msg m ++ "    " ++ history m

signMessage n = forever $ do
                delayThread 1
                m <- await
                yield $ m { history = (history m) ++ "-> " ++ show n}

human_send h r_writer   = runEffect $ getMessage h
                        >-> signMessage (name h)
                        >-> toOutput r_writer

human_recv h h_reader   = runEffect $ fromInput h_reader
                        >-> signMessage (name h)
                        >-> printMessage

router_route r r_reader h_writers = runEffect $ fromInput r_reader
                                    >-> signMessage (name r)
                                    >-> toOutput (choose_output h_writers $ table r)



choose_output :: [smth] -> [Int] -> smth
choose_output outputs table = outputs !! 0



get_rand_num :: Int -> Int -> R.StdGen -> (Int, R.StdGen)
get_rand_num x y g = R.randomR (x, y-1) g

get_rand_elem :: [Int] -> R.StdGen -> (Int, R.StdGen)
get_rand_elem l g = (l !! i, g')
                    where (i, g') = get_rand_num 0 (length l) g

get_rand_sublist :: [Int] -> Int -> R.StdGen -> ([Int], R.StdGen)
get_rand_sublist l max_links g = work l g n
                       where
                           (n, _) = get_rand_num 2 max_links g
                           work l g 0 = ([], g)
                           work l g n = (x:l', g'')
                               where
                                   (x, g') = get_rand_elem l g
                                   (l', g'') = work (substract x l) g' (n-1)

substract x l = filter (/= x) l

generate_routers :: Int -> [Int] -> [Int] -> Int -> R.StdGen -> ([Router], R.StdGen)
generate_routers 0 names l max_links g = ([], g)
generate_routers n names l max_links g = (Router { rname = head names
                                                 , routs = rts
                                                 , table = [ x | (_, x) <- zip names (cycle rts) ]
                                                 }
                                                 :l', g'')
                                        where
                                            (rts, g') = get_rand_sublist (substract (head names) l) max_links g
                                            (l', g'') = generate_routers (n-1) (tail names) l max_links g'

generate_humans :: Int -> [Int] -> [Int] -> R.StdGen -> ([Human], R.StdGen)
generate_humans 0 names l g = ([], g)
generate_humans n names l g = (Human { hname = head names
                                     , houts = hts
                                     , gen_rate = delay
                                     }
                                     :l', g'')
                            where
                                (hts, g') = get_rand_elem (substract (head names) l) g
                                (delay, _) = get_rand_num 0 5 g
                                (l', g'') = generate_humans (n-1) (tail names) l g'


generate_agents :: Int -> Int -> ([Human], [Router])
generate_agents num_humans num_routers  = (humans, routers)
    where
        num = num_humans + num_routers
        max_links = 4   -- max router links
        g = R.mkStdGen 0

        names = [x | x <- [0 .. num_humans + num_routers-1]]
        h_names = take num_humans names
        r_names = drop num_humans names

        (humans, _) = generate_humans num_humans h_names r_names g
        (routers, _) = generate_routers num_routers r_names names max_links g



main = do
    let
        writer = fst
        reader = snd
        num_humans = 5
        num_routers = 2
        num = num_humans + num_routers
        (humans, routers) = generate_agents num_humans num_routers

    print $ map hname humans
    print $ map houts humans
    print $ map rname routers
    print $ map routs routers

    pipes <- sequence $ map (\_ -> spawn unbounded) [0..(num-1)]

    let
        human_reader_pipes = [reader $ pipes !! i | i <- [0..(num_humans-1)]]
        human_writer_pipes = [writer $ pipes !! (houts (humans !! i)) | i <- [0..(num_humans-1)]]

        router_reader_pipes = [reader $ pipes !! i | i <- [num_humans..(num-1)]]
        router_writer_pipes = [[writer $ pipes !! j | j <- friends] | friends <- (map routs routers)]


    h_recv_tasks <- sequence $ [async $ task | task <- zipWith human_recv humans human_reader_pipes]
    h_send_tasks <- sequence $ [async $ task | task <- zipWith human_send humans human_writer_pipes]
    r_route_tasks <- sequence $ [async $ task | task <- zipWith3 router_route routers router_reader_pipes router_writer_pipes]

    waitAny h_send_tasks




