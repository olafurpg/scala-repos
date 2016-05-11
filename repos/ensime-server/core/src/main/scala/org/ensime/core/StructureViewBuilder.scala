// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.SourceFile
import org.ensime.api._

trait StructureViewBuilder {
  self: RichPresentationCompiler =>

  case class DefsBuilder(
      keyword: String,
      name: String,
      pos: SourcePosition,
      members: ListBuffer[DefsBuilder]
  ) {
    def build: StructureViewMember =
      StructureViewMember(keyword, name, pos, members.map(_.build).toList)
  }

  class StructureTraverser() extends Traverser {
    val log = LoggerFactory.getLogger(getClass)

    val stucture = new ListBuffer[StructureViewMember]()

    override def traverse(tree: Tree): Unit = {
      val df = DefsBuilder("", "", EmptySourcePosition(), new ListBuffer())
      traverse(tree, df)
      stucture.appendAll(df.members.map(_.build))
    }

    def shouldShow(x: DefDef): Boolean =
      !(x.name == nme.CONSTRUCTOR || x.name == nme.MIXIN_CONSTRUCTOR || x.symbol.isAccessor)

    def pos(x: Symbol) =
      locateSymbolPos(x, PosNeededYes)
        .getOrElse(EmptySourcePosition())

    private def traverse(tree: Tree, parent: DefsBuilder): Unit = {
      tree match {
        case x: DefTree if x.symbol.isSynthetic =>
        case x: ImplDef =>
          val df = DefsBuilder(x.keyword, x.name.toString, pos(x.symbol), new ListBuffer())
          parent.members.append(df)
          x.impl.body.foreach(traverse(_, df))
        case x: DefDef if shouldShow(x) =>
          parent.members.append(DefsBuilder(x.keyword, x.name.toString, pos(x.symbol), new ListBuffer()))
        case x: TypeDef =>
          parent.members.append(DefsBuilder(x.keyword, x.name.toString, pos(x.symbol), new ListBuffer()))
        case _ =>
          tree.children.foreach(traverse(_, parent))
      }
    }
  }

  def structureView(fileInfo: SourceFile): List[StructureViewMember] = {
    def getStructureTree(f: SourceFile) = {
      val x = new Response[Tree]()
      askStructure(true)(f, x)
      x.get
    }

    getStructureTree(fileInfo) match {
      case Left(tree) =>
        val traverser = new StructureTraverser()
        traverser.traverse(tree)
        traverser.stucture.toList
      case Right(ex) => List.empty
    }
  }

}
