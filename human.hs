module Human where

import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

import Generators
import Message


h_service h input out_node = do
                                send_task <- async $ h_send_msg h out_node
                                recv_task <- async $ h_recv_msg h input
                                wait send_task

h_send_msg h out_node = runEffect $ gen_message h
                        >-> toOutput out_node

h_recv_msg h input = runEffect $ fromInput input
                        >-> print_message h


