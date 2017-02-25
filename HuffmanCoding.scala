abstract class CodeTree(w: Int) {
  val weight = w
}
case class Node(left: CodeTree, right: CodeTree,
                chars: List[Char], override val weight: Int) extends CodeTree(weight) {
  override def toString: String = "[" + chars + "]" + "(" + weight + ")"
}
case class Leaf(char: Char, override val weight: Int) extends CodeTree(weight) {
  override def toString: String = "{" + char + "}" + "(" + weight + ")"
}

def weight(tree: CodeTree): Int = tree match {
  case Node(_, _, _, w) => w
  case Leaf(_, w) => w
}

def chars(tree: CodeTree): List[Char] = tree match {
  case Node(_, _, c, _) => c
  case Leaf(c, _) => List(c)
}

def string2Chars(str: String): List[Char] = str.toList

def makeCodeTree(left: CodeTree, right: CodeTree) =
  Node(left, right, chars(left) ::: chars(right),
    weight(left) + weight(right))

//val sampleTree = makeCodeTree(makeCodeTree(Leaf('a', 1), Leaf('b', 1)), Leaf('c', 1))
//sampleTree.left.asInstanceOf[Node].left
//sampleTree.left.asInstanceOf[Node].right
//sampleTree.right

//def times(chars: List[Char]): List[(Char, Int)] = {
//  chars.distinct.map(u => count(u, chars))
//}
//def count(C: Char, l: List[Char]): (Char, Int) = l match {
//  case Nil => (C, 0)
//  case C :: Nil => (C, 1)
//  case C :: tl => (C, count(C, tl)._2 + 1)
//  case _ :: tl => count(C, tl)
//}

def times(ch: List[Char]): List[(Char, Int)] = {
  ch.groupBy(c => c).map(cs => (cs._1, cs._2.length)).toList
}

//print(times(List('f','f','f','f')))

def sort(list: List[CodeTree]): List[CodeTree] = list match {
  case Nil => Nil
  case h :: tl => insert(h, sort(tl))
}
def insert(h: CodeTree, list: List[CodeTree]): List[CodeTree] = list match {
  case Nil => List(h)
  case hl :: tl => {
    if (h.weight <= hl.weight) {
      return h :: list
    } else {
      return hl :: insert(h, tl)
    }
  }
}

//sort(List(('h',7),('k',2),('g',4),('f',3)))
//List(('h',7),('k',2),('g',4),('f',3)).sortWith((i, j) => i._2 <= j._2)

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[CodeTree] = {
  sort(freqs.map(it => Leaf(it._1, it._2)))
}

//makeOrderedLeafList(List(('h',7),('k',2),('g',4),('f',3)))

def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
  case Nil => trees
  case _ :: Nil => trees
  case h1 :: h2 :: tl => {
    val node = makeCodeTree(h1, h2)
    insert(node, combine(tl))
  }
}

//val chs = string2Chars("abcdefgabcd")
//val leaves = makeOrderedLeafList(times(chs))
//combine(leaves)

// until(singleton, combine)(trees)
def until(singleton: (List[CodeTree]) => Boolean,
          combine: (List[CodeTree]) => List[CodeTree]):
((List[CodeTree]) => List[CodeTree]) = {

  def done(trees: List[CodeTree]): List[CodeTree] = singleton(trees) match {
    case true => trees
    case false => done(combine(trees))
  }
  done
}
//val chs = string2Chars("abcdefgabcd")
//val nodes = makeOrderedLeafList(times(chs))
//until(singleton, combine)(nodes)

def createCodeTree(chars: List[Char]): CodeTree = {
  val nodes = makeOrderedLeafList(times(chars))
  until(singleton, combine)(nodes).head
}

//createCodeTree(string2Chars("abcdabcdab"))

type Bit = Int

type CodeTable = List[(Char, List[Bit])]
def codeBits(table: CodeTable)(char: Char): List[Bit] = {
  def cb(char: Char): List[Bit] = {
    table.filter(_._1 == char) match {
      case Nil => List()
      case h :: tl => h._2
    }
  }
  cb(char)
}

def convert(t: CodeTree): CodeTable = {
  def find(t: CodeTree, bts: List[Bit]): CodeTable = t match {
    case Leaf(c, _) => List((c, bts))
    case Node(l, r, _, _) => mergeCodeTables(find(l, bts :+ 0), find(r, bts :+ 1))
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a ::: b
  }

  find(t, List())
}

//val tree = createCodeTree(string2Chars("abcdabcdab"))
//convert(tree)
//val bits = List(0,0,1,0,0,1,1,1)
//decode(tree, bits)

def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  val codeTable = convert(tree)
  val charSeacher = codeBits(codeTable) _
  def encode(chars: List[Char]): List[Bit] = chars match {
    case Nil => println("Char Not found!!"); List()
    case h :: Nil => charSeacher(h)
    case h :: tl => charSeacher(h) ::: encode(tl)
  }
  encode(text)
}

def decoder(tree: CodeTree, bits: List[Bit]): List[Char] = {
  def traverse(tr: CodeTree, bt: List[Bit]): (Char, List[Bit]) = tr match {
    case Leaf(c, _) => (c, bt)
    case Node(l, r, _, _) => bt match {
      case 0 :: tl => traverse(l, tl)
      case 1 :: tl => traverse(r, tl)
    }
  }

  bits match {
    case Nil => List()
    case h :: tl => {
      val trav = traverse(tree, bits)
      trav._1 :: decoder(tree, trav._2)
    }
  }
}

val tree = createCodeTree(string2Chars("abcdefgabcdr"))
//convert(tree)
val encoder = quickEncode(tree) _
val en = encoder(string2Chars("darshan"))
decoder(tree, en)
