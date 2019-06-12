module Generators where

import qualified System.Random as R
import Control.Monad
import Control.Monad.State
import Data.List


data Sender = Sender { s_id   :: Int
                     , s_out  :: Int
                     , s_rate :: Int
                     }
            deriving (Eq, Show)

data Router = Router { r_id      :: Int
                     , r_outs    :: [Int]
                     , r_table   :: [(Int, Int)]
                     }
            deriving (Eq, Show)

data Message = Routing { n_table   :: [(Int, Int)]
                       , n_source  :: Int
                       }
             | Ping { m_msg     :: [Char]
                    , m_dest    :: Int
                    }
            deriving (Eq, Show)


symsender = " |"
symRouter = " "
nINF = 1000

gimme_seed = R.mkStdGen 0


gen_rand_num :: Int -> Int -> State R.StdGen Int
gen_rand_num lower upper = state $ R.randomR (lower, upper-1)

gen_rand_elem :: [Int] -> State R.StdGen Int
gen_rand_elem l = gen_rand_num 0 (length l) >>= \i -> return $ l!!i

gen_rand_sublist :: [Int] -> Int -> State R.StdGen [Int]
gen_rand_sublist l 0 = return []
gen_rand_sublist l n = do
                        x <- gen_rand_elem l
                        l' <- gen_rand_sublist (substract x l) (n-1)
                        return $ x:l'


build_router :: Int -> [Int] -> [(Int,Int)] -> Router
build_router id outs table = Router { r_id = id
                                    , r_outs = outs
                                    , r_table = table
                                    }

build_sender :: Int -> Int -> Int -> Sender
build_sender id out r = Sender { s_id = id
                             , s_out = out
                             , s_rate = r
                             }


gen_senders :: [Int] -> [Int] -> [Sender]
gen_senders s_ids links = zipWith3 build_sender s_ids links rates
                            where
                                rates = evalState (replicateM (length s_ids) (gen_rand_num 1 10)) gimme_seed

gen_routers :: [Int] -> [[Int]] -> [Router]
gen_routers r_ids links = (zipWith3 build_router r_ids links tables)
                            where
                                tables = zipWith init_table (repeat $ last r_ids) links -- last r_ids ~ num_agents


init_table :: Int -> [Int] -> [(Int, Int)]
init_table n links = map (fn links) [0..n]
                where
                    fn :: [Int] -> Int -> (Int, Int)
                    fn x i
                        | elem i x  = (i, 1)
                        | otherwise = (last x, nINF) -- default route


gen_links :: [Int] -> [Int] -> ([Int], [[Int]])
gen_links s_ids r_ids = evalState (do
                                    -- let index x = x - length s_ids
                                    --     add_edges links i  = case length $ links!!i of
                                    --                            0 -> (add_edge links i) >>= \x -> add_edge x i
                                    --                            1 -> add_edge links i
                                    --                            _ -> return $ links
                                    --     add_edge links i = do
                                    --                         let e = r_ids !! i
                                    --                             used_nodes = e:(links!!i)
                                    --                             free_nodes = substract_all used_nodes r_ids
                                    --                         e' <- gen_rand_elem free_nodes
                                    --                         return $ insert_into e (index e') $ insert_into e' i links

                                    -- s_links <- replicateM (length s_ids) (gen_rand_elem r_ids)

                                    -- let base = [[] | _ <- r_ids]
                                    --     r_links_to_senders = foldr ($) base $ zipWith insert_into s_ids (map index s_links)
                                    -- r_links_to_routers <- foldM add_edges base [0..(length r_ids)-1]

                                    -- let r_links = zipWith (++) r_links_to_senders r_links_to_routers

                                    -- -- --         0, 1, 2, 3, 4, 5, ...
                                    -- -- s_links = [3, 7, 8]
                                    -- -- r_links = [ [4, 5, 0] -- 3
                                    -- --           , [3, 6] -- 4
                                    -- --           , [3, 7, 8] -- 5
                                    -- --           , [4, 7] -- 6
                                    -- --           , [5, 6, 8, 1] -- 7
                                    -- --           , [7, 5, 2] -- 8
                                    -- --           ]

                                    let s_links = [10, 13, 3]
                                        r_links = [ [2, 6, 4 ]
                                                  , [3, 5 ]
                                                  , [4, 8, 10 ]
                                                  , [3, 7, 8 ]
                                                  , [6, 8, 13 ]
                                                  , [5, 6, 7, 9 ]
                                                  , [8, 10 ]
                                                  , [0, 5, 9, 11 ]
                                                  , [10, 12 ]
                                                  , [11, 13 ]
                                                  , [1, 7, 12 ]
                                                  ]

                                    return (s_links, r_links)
                                ) gimme_seed


insert_into :: Int -> Int -> [[Int]] -> [[Int]]
insert_into x i links = take i links ++ [x:(links !! i)] ++ drop (i+1) links

substract :: Int -> [Int]-> [Int]
substract x l = filter (/=x) l

substract_all :: [Int] -> [Int]-> [Int]
substract_all [] l = l
substract_all (x:xs) l = substract_all xs $ substract x l


