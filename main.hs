import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)


data Human = Human { hname :: Int
                   , houts :: [Int]
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

class Outs x where
    outs :: x -> [Int]
instance Outs Human where outs = houts
instance Outs Router where outs = routs


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
                                    >-> toOutput $ choose_output h_writers $ table r


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
                                     , houts = [hts]
                                     , gen_rate = delay
                                     }
                                     :l', g'')
                            where
                                (hts, g') = get_rand_elem (substract (head names) l) g
                                (delay, _) = get_rand_num 0 5 g
                                (l', g'') = generate_humans (n-1) (tail names) l g'


generate_agents ::
generate_agents  = do
    let
        num_humans = 10
        num_routers = 25
        num = num_humans + num_routers
        max_links = 4   -- max router links
        g = R.mkStdGen 0

        names = [x | x <- [1 .. num_humans + num_routers]]
        h_names = take num_humans names
        r_names = drop num_humans names

        (humans, _) = generate_humans num_humans h_names r_names g
        (routers, _) = generate_routers num_routers r_names names max_links g


    print $ h_names
    print $ r_names
    print $ map outs humans
    print $ map outs routers

    return (humans, routers)





main = do
    let
        writer = fst
        reader = snd

    (humans, routers) = generate_agents

    pipes <- sequence $ map (\_ -> spawn unbounded) [0..num]

    h_recv_tasks <- sequence $ [async $ human_recv h (reader $ pipes !! i) | (h, i) <- zip humans [0..] ]
    h_send_tasks <- sequence $ [async $ human_send h (writer $ pipes !! r) | (h, i, [r]) <- zip3 humans [0..] $ map outs humans ]
    r_route_tasks <- sequence $ [async $ router_route r (reader $ pipes !! (num_humans+i+1)) [writer $ pipes !! j | j <- friends] | (r, i, friends) <- zip3 routers [0..] $ map outs routers ]

    waitAny h_send_tasks




