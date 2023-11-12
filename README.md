# Functional programming. Assignment # 2, Data structures.

Вариант: Tree-based Map

## Описание задания, цели и требования

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

Требования:

- Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть моноидом.

- Структуры данных должны быть неизменяемыми.

- Библиотека должна быть протестирована в рамках unit testing.

- Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
- Структура должна быть полиморфной.

Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.


## Реализация

Т.к наша структура является бинарным деревом, то определим ее как тип данных с двумя конструкторами:

```data Tree k v``` может быть:

1. ```Nil``` (ничего нет, пустой лист)
2. ```Node {keyvalue :: (k, v), leftchild :: Tree k v, rightchild :: Tree k v}``` (вершина дерева, имеющая конкретное значение, а именно - пару ключ-значение, дети - тоже деревья, поэтому могут быть как ```Nil```, так и ```Node```)

Структура является полиморфной с единственным требованием: тип ключа должен реализовывать отношение порядка.

### Добавление и удаление элементов

```haskell
--- PUT ---

put :: (Ord k) => k -> v -> Tree k v -> Tree k v
put k v Nil = Node (k, v) Nil Nil
put k v (Node kv@(k', v') left right)
  | k == k' = Node (k, v) left right
  | k < k' = Node kv (put k v left) right
  | k > k' = Node kv left (put k v right)

putKV :: (Ord k) => (k, v) -> Tree k v -> Tree k v
putKV (k, v) = put k v

putIf :: (Ord k) => (k -> Bool) -> k -> v -> Tree k v -> Tree k v
putIf pred k v tree = if pred k then put k v tree else tree
```

Обычная вставка в BST с сохранением его свойств + вспомогательные функции для удобства

```haskell
--- REMOVE ---

-- If the key to remove is less/greater than the current node -> recursively call left/right node
-- If the key to remove is equal to the current node -> call removeRoot
removeKey :: (Ord k) => k -> Tree k v -> Tree k v
removeKey _ Nil = Nil
removeKey k n@(Node kv@(k', _) left right)
  | k == k' = removeRoot n
  | k < k' = Node kv (removeKey k left) right
  | k > k' = Node kv left (removeKey k right)

-- In case node has no successors -> just remove
-- In case node has one successor -> swap with the successor and remove
-- In case node has two successors -> swap with the inorder (leftmost in the right child) successor and remove
removeRoot :: Tree k v -> Tree k v
removeRoot Nil = Nil
removeRoot (Node _ Nil Nil) = Nil
removeRoot (Node _ left Nil) = left
removeRoot (Node _ Nil right) = right
removeRoot (Node kv left right) = Node kv' left right'
  where
    kv' = keyvalue $ leftmost right
    right' = removeLeftmost right

leftmost Nil = Nil
leftmost n@(Node _ Nil _) = n
leftmost (Node _ left _) = leftmost left

removeLeftmost Nil = Nil
removeLeftmost (Node _ Nil right) = right
removeLeftmost (Node kv left right) = Node kv (removeLeftmost left) right
```

Немного хитрое, но базовое удаление из BST. Сравнивая ключ ходим по дереву то влево, то вправо, пока не найдем искомый. Если нашли, то удаляем по следующему принципу:

1. У вершины нет детей - просто удаляем
2. У вершины только один потомок - меняем их местами и удаляем
3. У вершины два потомка - берем самую левую вершину из правого поддерева (по свойствам BST это будет наименьший ключ больше удаляемого), меняем их местами и удаляем вершину. 

## Фильтрация

```haskell
--- FILTER ---

filterMap :: (Ord k) => (k -> Bool) -> Tree k v -> Tree k v
filterMap predi tree = filterHelper predi tree Nil
  where
    filterHelper _ Nil new = new
    filterHelper predi' (Node (k, v) left right) new =
      let filterLeft = filterHelper predi' left (putIf predi' k v new)
       in filterHelper predi' right filterLeft
```

Построение нового дерева на базе старого с использование putIf на заданный предикат.

## Отображение

```haskell
--- Map ---

mapMap :: (Ord k, Ord k0) => ((k, v) -> (k0, v0)) -> Tree k v -> Tree k0 v0
mapMap mapper node = helperMap mapper node Nil

helperMap :: (Ord k, Ord k0) => ((k, v) -> (k0, v0)) -> Tree k v -> Tree k0 v0 -> Tree k0 v0
helperMap _ Nil new = new
helperMap mapper (Node kv left right) new =
  let leftResult = helperMap mapper left new
   in helperMap mapper right (putKV (mapper kv) leftResult)
```

Построение нового дерева на базе старого с применением функции отображения перед вставкой.

## Свертки

```haskell
--- FOLDS ---

foldlMap :: (Ord k) => (b -> (k, v) -> b) -> b -> Tree k v -> b
foldlMap _ ini Nil = ini
foldlMap f ini (Node kv left right) =
  let leftResult = foldlMap f (f ini kv) left
   in foldlMap f leftResult right

foldrMap :: (Ord k) => ((k, v) -> b -> b) -> b -> Tree k v -> b
foldrMap _ ini Nil = ini
foldrMap f ini (Node kv left right) =
  let rightResult = kv `f` foldrMap f ini right
   in foldrMap f rightResult left
```

## Proprerty-based testing

Все тесты (включая unit) находятся в ```test/Test.hs```

При PBT использовалась библиотека QuickCheck.

Проверялись следующие свойства:

- Структура - моноид
    - Есть бинарная ассоциативная операция (слияния)
    - Есть нейтральный элемент (Nil), не меняющий структуру
- Дерево, отфильтрованное по некоторому предикату, все еще содержит все ключи, удовлетворяющие предикату и не содержит ни одного ключа, не удовлетворяющего предикату
- Правая свертка дерева, где сворачивающей функцией является добавление ключа очередного элемента в начало списка, а начальное значение - это пустой список, дает нам отсортированный по возрастанию список всех ключей дерева.

Результат тестирования можно посмотреть в workflow

## Выводы

В ходе выполнения лабораторной работы были изучены принципы построения структур данных в Haskell. Был объявлен тип данных Tree с двумя конструкторами и реализован базовый интерфейс для этой структуры данных. Также новым оказалось понятие Property-based testing, где структура данных тестировалась на наличие определенных свойств (хотя, насколько я понял, в Haskell нет трушного PBT, а QuickCheck лишь проверяет структуру на неизвестных нам различных наборах входных данных)