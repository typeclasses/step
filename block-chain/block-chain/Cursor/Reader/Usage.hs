module Cursor.Reader.Usage where

import Essentials
import Cursor.Reader.Type
import Cursor.Interface.Type
import Control.Monad.State
import Cursor.Interface.Orders

import Block (Block)
import Data.Function (flip)
import SupplyChain ((>-), (>->))

import qualified Cursor.Feed.Examples as Feed
import qualified SupplyChain.Job as Job
import qualified SupplyChain.Effect as Effect
import qualified Pushback
import qualified Optics
import qualified Pushback.Stack

readBlockList :: Block item block =>
    Reader (Const Void) 'Write item block product
    -> [block] -> (product, [block])
readBlockList (Reader r) xs = flip runState xs do
    Job.run $
        Pushback.Stack.substate (Optics.castOptic Optics.simple) Pushback.list
        >-> Feed.pushback
        >- (Job.alter' (Effect.alterPerform' (absurd . getConst)) r <* flush)

readBlockListM :: Block item block => Monad action =>
    Reader action 'Write item block product
    -> [block] -> action (product, [block])
readBlockListM (Reader r) xs = flip runStateT xs do
    Job.run $
        Pushback.Stack.substate (Optics.castOptic Optics.simple) Pushback.list
        >-> Feed.pushback
        >- (Job.alter' (Effect.alterPerform' lift) r <* flush)
