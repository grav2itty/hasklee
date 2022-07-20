module Hasklee.Mesh.New where

import Data.Maybe
import Data.Foldable
import qualified Data.Vector as V
import Linear

import Hgal.Data.SurfaceMesh
import Hgal.Graph.Loops

import qualified Hasklee.Mesh as Old
import qualified Hasklee.Vertex as Hasklee

newtype Mesh a = Mesh (SurfaceMesh (Hasklee.Vertex a) ())


instance (Eq a, Epsilon a, Floating a) => Old.ToMesh (Mesh a) a where
  toMesh (Mesh sm) =
    let verts f = fromMaybe zero . (\(Vertex i) -> pp V.! i) . vertex sm <$>
          halfedgesAroundFace sm (halfedge sm f)
        pp = pointProperties sm
    in foldMap (Old.primitive . toList . verts) (faces sm)


