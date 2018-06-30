module Halogen.VDom.Util
  ( effPure
  , effUnit
  , newMutMap
  , pokeMutMap
  , deleteMutMap
  , unsafeFreeze
  , unsafeLookup
  , unsafeGetAny
  , unsafeHasAny
  , unsafeSetAny
  , unsafeDeleteAny
  , forE
  , forInE
  , replicateE
  , diffWithIxE
  , diffWithKeyAndIxE
  , strMapWithIxE
  , refEq
  , createTextNode
  , setTextContent
  , createElement
  , insertChildIx
  , removeChild
  , unsafeParent
  , setAttribute
  , removeAttribute
  , addEventListener
  , removeEventListener
  , JsUndefined
  , jsUndefined
  ) where

import Prelude
import Effect (Effect)
import Data.Function.Uncurried as Fn
import Data.Nullable (Nullable)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STObject
import Web.Event.EventTarget (EventListener) as DOM
import Web.DOM (Document, Element, Node) as DOM
import Halogen.VDom.Types (Namespace, ElemName)
import Unsafe.Coerce (unsafeCoerce)

effPure ∷ ∀ a. a → Effect a
effPure = pure

effUnit ∷ Effect Unit
effUnit = pure unit

newMutMap ∷ ∀ r a. Effect (STObject r a)
newMutMap = unsafeCoerce STObject.new

pokeMutMap ∷ ∀ r a. Fn.Fn3 String a (STObject r a) (Effect Unit)
pokeMutMap = unsafeSetAny

deleteMutMap ∷ ∀ r a. Fn.Fn2 String (STObject r a) (Effect Unit)
deleteMutMap = unsafeDeleteAny

unsafeFreeze ∷ ∀ r a. STObject r a → Object a
unsafeFreeze = unsafeCoerce

unsafeLookup ∷ ∀ a. Fn.Fn2 String (Object a) a
unsafeLookup = unsafeGetAny

foreign import unsafeGetAny
  ∷ ∀ a b. Fn.Fn2 String a b

foreign import unsafeHasAny
  ∷ ∀ a. Fn.Fn2 String a Boolean

foreign import unsafeSetAny
  ∷ ∀ a b. Fn.Fn3 String a b (Effect Unit)

foreign import unsafeDeleteAny
  ∷ ∀ a. Fn.Fn2 String a (Effect Unit)

foreign import forE
  ∷ ∀ a b
  . Fn.Fn2
      (Array a)
      (Fn.Fn2 Int a (Effect b))
      (Effect (Array b))

foreign import forInE
  ∷ ∀ a
  . Fn.Fn2
      (Object.Object a)
      (Fn.Fn2 String a (Effect Unit))
      (Effect Unit)

foreign import replicateE
  ∷ ∀ a
  . Fn.Fn2
      Int
      (Effect a)
      (Effect Unit)

foreign import diffWithIxE
  ∷ ∀ b c d
  . Fn.Fn5
      (Array b)
      (Array c)
      (Fn.Fn3 Int b c (Effect d))
      (Fn.Fn2 Int b (Effect Unit))
      (Fn.Fn2 Int c (Effect d))
      (Effect (Array d))

foreign import diffWithKeyAndIxE
  ∷ ∀ a b c d
  . Fn.Fn6
      (Object.Object a)
      (Array b)
      (b → String)
      (Fn.Fn4 String Int a b (Effect c))
      (Fn.Fn2 String a (Effect d))
      (Fn.Fn3 String Int b (Effect c))
      (Effect (Object.Object c))

foreign import strMapWithIxE
  ∷ ∀ a b
  . Fn.Fn3
      (Array a)
      (a → String)
      (Fn.Fn3 String Int a (Effect b))
      (Effect (Object.Object b))

foreign import refEq
  ∷ ∀ a b. Fn.Fn2 a b Boolean

foreign import createTextNode
  ∷ Fn.Fn2 String DOM.Document (Effect DOM.Node)

foreign import setTextContent
  ∷ Fn.Fn2 String DOM.Node (Effect Unit)

foreign import createElement
  ∷ Fn.Fn3 (Nullable Namespace) ElemName DOM.Document (Effect DOM.Element)

foreign import insertChildIx
  ∷ Fn.Fn3 Int DOM.Node DOM.Node (Effect Unit)

foreign import removeChild
  ∷ Fn.Fn2 DOM.Node DOM.Node (Effect Unit)

foreign import unsafeParent
  ∷ DOM.Node → DOM.Node

foreign import setAttribute
  ∷ Fn.Fn4 (Nullable Namespace) String String DOM.Element (Effect Unit)

foreign import removeAttribute
  ∷ Fn.Fn3 (Nullable Namespace) String DOM.Element (Effect Unit)

foreign import addEventListener
  ∷ Fn.Fn3 String DOM.EventListener DOM.Element (Effect Unit)

foreign import removeEventListener
  ∷ Fn.Fn3 String DOM.EventListener DOM.Element (Effect Unit)

foreign import data JsUndefined ∷ Type

foreign import jsUndefined ∷ JsUndefined
