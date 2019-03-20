module Generators where

import qualified System.Random as R
import Control.Monad
import Data.List


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

data Message' = Routing  { n_table   :: [(Int, Int)]
                        , n_source  :: Int
                        }
             | Ping     { m_msg     :: [Char]
                        , m_dest    :: Int
                        }
            deriving (Eq, Show)

data Message = Message { msg    :: Message'
                       , trace  :: [Char]
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
gen_rand_sublist l n g = (x:l', g'')
                        where
                            (x, g') = gen_rand_elem l g
                            (l', g'') = gen_rand_sublist (substract x l) (n-1) g'


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
gen_humans h_ids r_ids g = (zipWith3 make_human h_ids links rates, g'')
                        where
                            lists = map (flip substract r_ids) h_ids
                            (links, g') = mapr (\(e, g) -> gen_rand_elem e g) lists g
                            (rates, g'') = mapr (\(_, g) -> gen_rand_num 0 5 g) h_ids g'


gen_routers :: [Int] -> [[Int]] -> R.StdGen -> ([Router], R.StdGen)
gen_routers r_ids links g = (zipWith3 make_router r_ids links tables, g)
                                    where
                                        tables = zipWith init_table (repeat $ (length links -1)) links


gen_links :: Int -> [Int] -> [Int] -> R.StdGen -> ([[Int]], R.StdGen)
gen_links nlinks h_ids r_ids g = (outs, g')
                                    where
                                        ids = h_ids ++ r_ids
                                        (r_links, g') = mapr (\(_, _g) -> gen_rand_sublist r_ids 2 _g) [0..nlinks] g
                                        h_links = [ [e, l] | (e, l) <- zip h_ids $ cycle r_ids ]
                                        rr_links = [ [e, l] | (e, l) <- zip (tail r_ids ++ [head r_ids]) $ cycle r_ids ]

                                        links = h_links ++ r_links ++ rr_links

                                        -- g' = g
                                        -- links = h_links ++ rr_links

                                        llinks = map (nub . concat . (flip  filter) links . elem) ids
                                        outs = drop (length h_ids) $ zipWith substract ids llinks


init_table :: Int -> [Int] -> [(Int, Int)]
init_table l out = map (f out) [0..l]

f :: [Int] -> Int -> (Int, Int)
f x i
    | elem i x  = (i, 1)
    | otherwise = (last x, nINF)


lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

gimme_seed = R.mkStdGen 0

gen_agents :: Int -> Int -> ([Human], [Router])
gen_agents num_humans num_routers  = (humans, routers)
    where
        nlinks = 8

        ids = [0..(num_humans + num_routers-1)]
        h_ids = take num_humans ids
        r_ids = drop num_humans ids

        g = gimme_seed
        (links, _) = gen_links nlinks h_ids r_ids g
        (humans, _) = gen_humans h_ids r_ids g
        (routers, _) = gen_routers r_ids links g

