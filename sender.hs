module Sender where

import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

import Generators
import Message


s_service s input out_node = do
                                send_task <- async $ s_send_msg s out_node
                                recv_task <- async $ s_recv_msg s input
                                wait send_task

--- create a message and pass it to the linked router
s_send_msg s out_node = runEffect $ gen_message s
                        >-> toOutput out_node

--- receive a message and print it
s_recv_msg s input = runEffect $ fromInput input
                        >-> print_message s

