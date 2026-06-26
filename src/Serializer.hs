{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Aeson JSON serialization for the Syntax AST.


module Serializer () where

import Data.Aeson hiding (Array)
import Data.Text qualified as T (Text)
import Syntax qualified


-- ---------------------------------------------------------------------------
-- Dim
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.Dim T.Text) where
  toJSON (Syntax.DimVar v) = object ["tag" .= ("DimVar" :: T.Text), "var"  .= v]
  toJSON (Syntax.DimN   n) = object ["tag" .= ("DimN"   :: T.Text), "n"    .= n]
  toJSON (Syntax.Add  ds)  = object ["tag" .= ("Add"    :: T.Text), "dims" .= ds]
  toJSON (Syntax.Mul  ds)  = object ["tag" .= ("Mul"    :: T.Text), "dims" .= ds]
  toJSON (Syntax.Sub  ds)  = object ["tag" .= ("Sub"    :: T.Text), "dims" .= ds]

-- ---------------------------------------------------------------------------
-- Shape
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.Shape T.Text) where
  toJSON (Syntax.ShapeVar v)    = object ["tag" .= ("ShapeVar" :: T.Text), "var"    .= v]
  toJSON (Syntax.ShapeDim d)    = object ["tag" .= ("ShapeDim" :: T.Text), "dim"    .= d]
  toJSON (Syntax.Concat  shapes) = object ["tag" .= ("Concat"  :: T.Text), "shapes" .= shapes]

-- ---------------------------------------------------------------------------
-- Extent
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.Extent T.Text) where
  toJSON (Syntax.Dim   d) = object ["tag" .= ("Dim"   :: T.Text), "dim"   .= d]
  toJSON (Syntax.Shape s) = object ["tag" .= ("Shape" :: T.Text), "shape" .= s]

-- ---------------------------------------------------------------------------
-- ExtentParam
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.ExtentParam T.Text) where
  toJSON (Syntax.DimParam   v) = object ["tag" .= ("DimParam"   :: T.Text), "var" .= v]
  toJSON (Syntax.ShapeParam v) = object ["tag" .= ("ShapeParam" :: T.Text), "var" .= v]

-- ---------------------------------------------------------------------------
-- TypeParam
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.TypeParam T.Text) where
  toJSON (Syntax.AtomTypeParam  v) = object ["tag" .= ("AtomTypeParam"  :: T.Text), "var" .= v]
  toJSON (Syntax.ArrayTypeParam v) = object ["tag" .= ("ArrayTypeParam" :: T.Text), "var" .= v]

-- ---------------------------------------------------------------------------
-- TypeExp
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.TypeExp T.Text) where
  toJSON (Syntax.TEAtomVar  v _)    = object ["tag" .= ("TEAtomVar"  :: T.Text), "var"    .= v]
  toJSON (Syntax.TEArrayVar v _)    = object ["tag" .= ("TEArrayVar" :: T.Text), "var"    .= v]
  toJSON (Syntax.TEBool       _)    = object ["tag" .= ("TEBool"     :: T.Text)]
  toJSON (Syntax.TEInt        _)    = object ["tag" .= ("TEInt"      :: T.Text)]
  toJSON (Syntax.TEFloat      _)    = object ["tag" .= ("TEFloat"    :: T.Text)]
  toJSON (Syntax.TEArray  t s _)    = object ["tag" .= ("TEArray"    :: T.Text), "elem"   .= t,  "shape"  .= s]
  toJSON (Syntax.TEArrow  ts t _)   = object ["tag" .= ("TEArrow"    :: T.Text), "in"     .= ts, "out"    .= t]
  toJSON (Syntax.TEForall ps t _)   = object ["tag" .= ("TEForall"   :: T.Text), "params" .= ps, "body"   .= t]
  toJSON (Syntax.TEPi     ps t _)   = object ["tag" .= ("TEPi"       :: T.Text), "params" .= ps, "body"   .= t]
  toJSON (Syntax.TESigma  ps t _)   = object ["tag" .= ("TESigma"    :: T.Text), "params" .= ps, "body"   .= t]

-- ---------------------------------------------------------------------------
-- Base
-- ---------------------------------------------------------------------------
instance ToJSON Syntax.Base where
  toJSON (Syntax.BoolVal b)  = object ["tag" .= ("BoolVal"  :: T.Text), "value" .= b]
  toJSON (Syntax.IntVal  n)  = object ["tag" .= ("IntVal"   :: T.Text), "value" .= n]
  toJSON (Syntax.FloatVal f) = object ["tag" .= ("FloatVal" :: T.Text), "value" .= f]

-- ---------------------------------------------------------------------------
-- Pat
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.Pat Syntax.NoInfo T.Text) where
  toJSON :: Syntax.Pat Syntax.NoInfo T.Text -> Value
  toJSON (Syntax.PatId v t Syntax.NoInfo _) = object ["name" .= v, "type" .= t]

-- ---------------------------------------------------------------------------
-- Atom
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.Atom Syntax.NoInfo T.Text) where
  toJSON (Syntax.Base b Syntax.NoInfo _) =
    object ["tag" .= ("Base" :: T.Text), "value" .= b]
  toJSON (Syntax.Lambda ps body Syntax.NoInfo _) =
    object ["tag" .= ("Lambda"  :: T.Text), "params"  .= ps, "body" .= body]
  toJSON (Syntax.TLambda ps body Syntax.NoInfo _) =
    object ["tag" .= ("TLambda" :: T.Text), "params"  .= ps, "body" .= body]
  toJSON (Syntax.ILambda ps body Syntax.NoInfo _) =
    object ["tag" .= ("ILambda" :: T.Text), "params"  .= ps, "body" .= body]
  toJSON (Syntax.Box extents body t Syntax.NoInfo _) =
    object ["tag" .= ("Box"     :: T.Text), "extents" .= extents, "body" .= body, "type" .= t]

-- ---------------------------------------------------------------------------
-- Bind
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.Bind Syntax.NoInfo T.Text) where
  toJSON (Syntax.BindVal v mt body _) =
    object ["tag" .= ("BindVal"    :: T.Text), "name" .= v, "type" .= mt, "body" .= body]
  toJSON (Syntax.BindType tp ty Syntax.NoInfo _) =
    object ["tag" .= ("BindType"   :: T.Text), "param" .= tp, "type" .= ty]
  toJSON (Syntax.BindExtent ep ext _) =
    object ["tag" .= ("BindExtent" :: T.Text), "param" .= ep, "extent" .= ext]
  toJSON (Syntax.BindFun v ps mt body Syntax.NoInfo _) =
    object ["tag" .= ("BindFun"    :: T.Text), "name" .= v, "params"  .= ps, "type" .= mt, "body" .= body]
  toJSON (Syntax.BindTFun v ps mt body Syntax.NoInfo _) =
    object ["tag" .= ("BindTFun"   :: T.Text), "name" .= v, "params" .= ps, "type" .= mt, "body" .= body]
  toJSON (Syntax.BindIFun v ps mt body Syntax.NoInfo _) =
    object ["tag" .= ("BindIFun"   :: T.Text), "name" .= v, "params" .= ps, "type" .= mt, "body" .= body]

-- ---------------------------------------------------------------------------
-- Exp
-- ---------------------------------------------------------------------------
instance ToJSON (Syntax.Exp Syntax.NoInfo T.Text) where
  toJSON (Syntax.Var v Syntax.NoInfo _) =
    object ["tag" .= ("Var" :: T.Text), "name" .= v]
  toJSON (Syntax.Array shape as Syntax.NoInfo _) =
    object ["tag" .= ("Array"      :: T.Text), "shape" .= shape, "elems"    .= as]
  toJSON (Syntax.EmptyArray shape t Syntax.NoInfo _) =
    object ["tag" .= ("EmptyArray" :: T.Text), "shape" .= shape, "elemType" .= t]
  toJSON (Syntax.Frame shape es Syntax.NoInfo _) =
    object ["tag" .= ("Frame"      :: T.Text), "shape" .= shape, "elems"    .= es]
  toJSON (Syntax.EmptyFrame shape t Syntax.NoInfo _) =
    object ["tag" .= ("EmptyFrame" :: T.Text), "shape" .= shape, "elemType" .= t]
  toJSON (Syntax.App f args Syntax.NoInfo _) =
    object ["tag" .= ("App"  :: T.Text), "fun" .= f, "args"    .= args]
  toJSON (Syntax.TApp f ts Syntax.NoInfo _) =
    object ["tag" .= ("TApp" :: T.Text), "fun" .= f, "args"   .= ts]
  toJSON (Syntax.IApp f extents Syntax.NoInfo _) =
    object ["tag" .= ("IApp" :: T.Text), "fun" .= f, "args" .= extents]
  toJSON (Syntax.Unbox eps v src body Syntax.NoInfo _) =
    object ["tag" .= ("Unbox" :: T.Text), "extents" .= eps, "name" .= v, "target" .= src, "body" .= body]
  toJSON (Syntax.Let binds body Syntax.NoInfo _) =
    object ["tag" .= ("Let"   :: T.Text), "binds" .= binds, "body" .= body]
