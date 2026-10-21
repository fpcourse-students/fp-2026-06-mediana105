-- | Домашка 6. Классы типов: обязательные задачи.
module Level1 where

import Data.Kind (Type)
import Examples qualified as E
import MetaUtils (todo, todoImpl)


-- 1.1. Прочитайте src/Examples.hs до запуска.
-- Запишите результаты литералами и конструкторами, без вычислений из условия.

-- E.render E.Warning
predictedShow :: String
predictedShow = "[WARNING]"

-- E.Labelled "old" 7 == E.Labelled "new" 7
predictedEq :: Bool
predictedEq = True

-- fmap not (Just True)
predictedFmap :: Maybe Bool
predictedFmap = Just False


-- 1.2. Равенство деревьев.
-- Сравните форму дерева и вторую компоненту пары в каждом узле.
-- Первая компонента пары — служебная метка, она не влияет на равенство.
-- Законы Eq — лекция 6; среди тестов есть Reflexivity, Symmetry, Transitivity.

data IntTree = IntLeaf | IntNode IntTree (Int, Int) IntTree
  deriving Show

{- HLINT ignore "Use record patterns" -}
instance Eq IntTree where
  IntLeaf == IntLeaf = True
  IntNode left1 (_, value1) right1 == IntNode left2 (_, value2) right2 = left1 == left2 && right1 == right2 && value1 == value2
  _ == _ = False


-- 1.3. Functor для дерева с произвольным числом детей.
-- Примените функцию к значению каждого Node. Форму и порядок детей сохраните.
-- deriving Functor вместо собственного инстанса использовать нельзя.

data Tree a = Leaf | Node a [Tree a]
  deriving (Eq, Show)

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node value tree) = Node (f value) (map (fmap f) tree)




-- 1.4. Сравнение структур.
-- Класс StructEq сравнивает форму контейнеров, игнорируя значения элементов.
-- Реализуйте оператор (==#) для списков и Tree, написав инстансы целиком.
-- Типы элементов двух контейнеров могут различаться.
-- Для списков одинаковая структура означает одинаковую длину.
-- Для Tree нужны одинаковые конструкторы и формы детей в том же порядке.

class StructEq (f :: Type -> Type) where
  (==#) :: f a -> f b -> Bool

instance StructEq [] where
  a ==# b = length a == length b


instance StructEq Tree where
  Leaf ==# Leaf = True
  Node _ tree1 ==# Node _ tree2 = length tree1 == length tree2 && and (zipWith (==#) tree1 tree2)
  _ ==# _ = False