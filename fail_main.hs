import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)


data Human = Human { h_id   :: Int
                   , h_out  :: Int
                   , h_rate :: Int
                   }

data Router = Router    { r_id      :: Int
                        , r_out     :: [Int]
                        , r_table   :: [(Int, Int)]
                        }

data Message = Message { m_msg      :: [Char]
                       , m_trace    :: [Char]
                       , m_dest     :: Int
                       }

-- class Name x where
--     name :: x -> Int
-- instance Name Human
--     where name = h_id
-- instance Name Router
--     where name = r_id


delayThread t = lift $ threadDelay (t * 100000)


get_message h = forever $ do
                delayThread (h_rate h)
                let m = Message { m_msg = "swap dis naw" , m_trace = "|", m_dest = (rem (h_rate h) 5)}
                yield m

print_message = forever $ do
                delayThread 1
                m <- await
                lift $ print $ m_msg m ++ "    " ++ m_trace m

sign_message n = forever $ do
                delayThread 1
                m <- await
                yield $ m { m_trace = (m_trace m) ++ "-> " ++ show n }


h_send h output = runEffect $ get_message h
                    >-> sign_message (h_id h)
                    >-> toOutput output

h_recv h input  = runEffect $ fromInput input
                    >-> sign_message (h_id h)
                    >-> print_message


r_route r input outputs = runEffect $ fromInput input
                            >-> sign_message (r_idname r)
                            >-> get_output outputs (r_table r)

get_output outs table = forever $ do
                        m <- await
                        let
                            -- out = outs !! (fst $ table !! (m_dest m))
                            out = outs !! 0
                        lift $ ignore_m $ atomically $ send out m

ignore_m a = do x<-a
                return ()




gen_rand_num :: Int -> Int -> R.StdGen -> (Int, R.StdGen)
gen_rand_num x y g = R.randomR (x, y-1) g

gen_rand_elem :: [Int] -> R.StdGen -> (Int, R.StdGen)
gen_rand_elem l g = (l !! i, g')
                    where (i, g') = gen_rand_num 0 (length l) g

gen_rand_sublist :: [Int] -> Int -> R.StdGen -> ([Int], R.StdGen)
gen_rand_sublist l max_links g = work l g n
                       where
                            (n, _) = gen_rand_num 2 max_links g
                            work l g 0 = ([], g)
                            work l g n = (x:l', g'')
                                where
                                    (x, g') = gen_rand_elem l g
                                    (l', g'') = work (substract x l) g' (n-1)

gen_routers :: Int -> [Int] -> [Int] -> Int -> R.StdGen -> ([Router], R.StdGen)
gen_routers 0 names l max_links g = ([], g)
gen_routers n names l max_links g = (Router { r_id = head names
                                                 , r_out = rts
                                                 , r_table = [ (x, 666) | (_, x) <- zip names (cycle rts) ]
                                                 }
                                                 :l', g'')
                                        where
                                            (rts, g') = gen_rand_sublist (substract (head names) l) max_links g
                                            (l', g'') = gen_routers (n-1) (tail names) l max_links g'

gen_humans :: Int -> [Int] -> [Int] -> R.StdGen -> ([Human], R.StdGen)
gen_humans 0 names l g = ([], g)
gen_humans n names l g = (Human { h_id = head names
                                     , h_out = hts
                                     , h_rate = delay
                                     }
                                     :l', g'')
                            where
                                (hts, g') = gen_rand_elem (substract (head names) l) g
                                (delay, _) = gen_rand_num 0 5 g
                                (l', g'') = gen_humans (n-1) (tail names) l g'

substract x l = filter (/=x) l

gen_agents :: Int -> Int -> ([Human], [Router])
gen_agents num_humans num_routers  = (humans, routers)
    where
        max_links = 4   -- max router links

        names = [0..(num_humans + num_routers-1)]
        h_names = take num_humans names
        r_names = drop num_humans names

        g = R.mkStdGen 0
        (humans, _) = gen_humans num_humans h_names r_names g
        (routers, _) = gen_routers num_routers r_names names max_links g



main = do
    let
        writer = fst
        reader = snd
        num_humans = 4
        num_routers = 5
        num = num_humans + num_routers
        (humans, routers) = gen_agents num_humans num_routers

    print $ map h_id humans
    print $ map h_out humans
    print $ map r_id routers
    print $ map r_out routers

    pipes <- sequence $ map (\_ -> spawn unbounded) [0..(num-1)]

    let
        h_reader_pipes = [reader $ pipes !! i | i <- [0..(num_humans-1)]]
        h_writer_pipes = [writer $ pipes !! (h_out (humans !! i)) | i <- [0..(num_humans-1)]]
        r_reader_pipes = [reader $ pipes !! i | i <- [num_humans..(num-1)]]
        r_writer_pipes = [[writer $ pipes !! j | j <- friends] | friends <- (map r_out routers)]

    h_recv_tasks <- sequence $ [async $ task | task <- zipWith h_recv humans h_reader_pipes]
    h_send_tasks <- sequence $ [async $ task | task <- zipWith h_send humans h_writer_pipes]
    r_route_tasks <- sequence $ [async $ task | task <- zipWith3 r_route routers r_reader_pipes r_writer_pipes]

    waitAny h_send_tasks


