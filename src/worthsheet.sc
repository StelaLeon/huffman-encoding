import patmat.Huffman.{Fork, CodeTree, Leaf}

def times(chars: List[Char]): List[(Char, Int)] = chars.distinct.map((c:Char)=>{
  (c,chars.count(_ == c))
})
times(List('a', 'b', 'a'))
def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  freqs.sortBy((f:(Char,Int))=>{f._2})
    .map((f:(Char,Int))=>Leaf(f._1,f._2))

val tmp = List(('a',7),('b',5))

tmp.sortBy(_._2)
makeOrderedLeafList(tmp)
//until(singleton, combine)(trees)


def singleton(trees: List[CodeTree]): Boolean = trees.tail.isEmpty


// Part 1: Basics
def weight(tree: CodeTree): Int = tree match{
  case tree: Leaf => tree.weight
  case tree: Fork => weight(tree.left) + weight(tree.right)
} // tree match

def chars(tree: CodeTree): List[Char] = tree match{
  case tree: Leaf => List(tree.char)
  case tree: Fork => chars(tree.left):::chars(tree.right)
}// tree match ...

def makeCodeTree(left: CodeTree, right: CodeTree) =
  Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

def combine(trees: List[CodeTree]): List[CodeTree] =
  makeCodeTree( trees.head,trees.tail.head) :: trees.tail.tail .sortBy(_ match{
    case l: Leaf => l.weight
    case f: Fork => f.weight
  })


def until(stopCondition: List[CodeTree] => Boolean, combineFunction: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
  if(stopCondition(trees))
    trees
  until(stopCondition,combineFunction)(trees)
}
val tmpTree = List(Leaf('e',1), Leaf('t',2), Leaf('x',3))

until(singleton,combine)(tmpTree)