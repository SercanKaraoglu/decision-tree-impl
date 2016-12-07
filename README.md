# decision-tree-impl

Scala implementation trial for Decision Tree

this is not just cool, tail-recursion in Scala is more than that.

```scala
def classify(tree: Tree, testSource: Vector[Row]): Vector[Any] = {
    @tailrec def classifier(t: Tree, observation: Row): Any = t match {
      case Node(attrId: Int, value: String, leftBranch: Tree, rightBranch: Tree) => if (value == observation.getString(attrId)) classifier(leftBranch, observation) else classifier(rightBranch, observation)
      case Node(attrId: Int, value: Double, leftBranch: Tree, rightBranch: Tree) => if (value > observation.getDouble(attrId)) classifier(leftBranch, observation) else classifier(rightBranch, observation)
      case NonEmptyLeaf(targetValues: ValueSet) => targetValues.head
      case EmptyLeaf() => Nil
    }

    testSource.map(classifier(tree, _))
 }
 ```
