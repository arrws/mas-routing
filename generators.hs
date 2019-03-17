module Generators where

import qualified System.Random as R
import Control.Monad


data Human = Human { h_id   :: Int
                   , h_out  :: Int
                   , h_rate :: Int
                   }
                   deriving (Eq, Show)

data Router = Router    { r_id      :: Int
                        , r_outs    :: [Int]
                        , r_table   :: [(Int, Int)]
                        }
                        deriving (Eq, Show)

data Message = Routing  { n_table   :: [(Int, Int)]
                        , n_source  :: Int
                        }
             | Ping     { m_msg     :: [Char]
                        , m_trace   :: [Char]
                        , m_dest    :: Int
                        }
            deriving (Eq, Show)


symHuman = " |"
symRouter = " "

nINF = 9999

gen_rand_num :: Int -> Int -> R.StdGen -> (Int, R.StdGen)
gen_rand_num lower upper g = R.randomR (lower, upper-1) g

gen_rand_elem :: [Int] -> R.StdGen -> (Int, R.StdGen)
gen_rand_elem l g = (l !! i, g')
                    where (i, g') = gen_rand_num 0 (length l) g

gen_rand_sublist :: [Int] -> Int -> R.StdGen -> ([Int], R.StdGen)
gen_rand_sublist l 0 g = ([], g)
gen_rand_sublist l n g = (x:l', g')
                        where
                            (x, _) = gen_rand_elem l g
                            (l', g') = gen_rand_sublist (substract x l) (n-1) g


substract x l = filter (/=x) l


make_router :: Int -> [Int] -> [(Int,Int)] -> Router
make_router id outs table = Router { r_id = id
                                   , r_outs = outs
                                   , r_table = table
                                   }

make_human :: Int -> Int -> Int -> Human
make_human id out r = Human { h_id = id
                            , h_out = out
                            , h_rate = r
                            }

mapr :: ((a, R.StdGen) -> (b, R.StdGen)) -> [a] -> R.StdGen -> ([b], R.StdGen)
mapr f list g = foldr (\e (acc, g) -> let (e', g') = f (e, g)
                                      in (e':acc, g')) ([], g) list

gen_humans :: [Int] -> [Int] -> R.StdGen -> ([Human], R.StdGen)
gen_humans ids outs g = (zipWith3 make_human ids outss rates, g'')
                        where
                            lists = map (flip substract outs) ids
                            (outss, g') = mapr (\(e, g) -> gen_rand_elem e g) lists g
                            (rates, g'') = mapr (\(_, g) -> gen_rand_num 0 5 g) ids g'


gen_routers :: [Int] -> [Int] -> Int -> R.StdGen -> ([Router], R.StdGen)
gen_routers ids outs nlinks g = (zipWith3 make_router ids outss tables, g')
                                    where
                                        tables = replicate (length ids) $ replicate (length ids) (head outs, nINF)
                                        lists = map (flip substract outs) ids
                                        (outss, g') = mapr (\(e, g) -> gen_rand_sublist e nlinks g) lists g


gen_agents :: Int -> Int -> ([Human], [Router])
gen_agents num_humans num_routers  = (humans, routers)
    where
        nlinks = 2

        ids = [0..(num_humans + num_routers-1)]
        h_ids = take num_humans ids
        r_ids = drop num_humans ids

        g = R.mkStdGen 2
        (humans, _) = gen_humans h_ids r_ids g
        (routers, _) = gen_routers r_ids ids nlinks g


