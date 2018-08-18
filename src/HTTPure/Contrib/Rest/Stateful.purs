module HTTPure.Contrib.Rest.Stateful
  ( readState
  , RO
  , readWriteState, readWriteState'
  , RW
  , withState
  ) where

import Prelude

import Data.Tuple (Tuple(Tuple))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

readState
  :: forall state request response
   . Ref state
  -> (Tuple state request -> Aff response)
  -> request
  -> Aff response
readState stateRef handler request = do
  state <- liftEffect $ Ref.read stateRef
  handler (Tuple state request)

type RO state =
  forall request response
   . (Tuple state request -> Aff response)
  -> request
  -> Aff response

readWriteState
  :: forall state request response
   . Ref state
  -> (Tuple state request -> Aff (Tuple state response))
  -> request
  -> Aff response
readWriteState stateRef = readWriteState' stateRef (const $ pure unit)

readWriteState'
  :: forall state request response
   . Ref state
  -> (state -> Aff Unit)
  -> (Tuple state request -> Aff (Tuple state response))
  -> request
  -> Aff response
readWriteState' stateRef after handler request = do
  state <- liftEffect $ Ref.read stateRef
  Tuple newState response <- handler (Tuple state request)
  liftEffect $ Ref.write newState stateRef
  after newState
  pure response

type RW state =
  forall request response
   . (Tuple state request -> Aff (Tuple state response))
  -> request
  -> Aff response

withState
  :: forall state response
   . state
  -> Aff response
  -> Aff (Tuple state response)
withState state =
  map (Tuple state)
