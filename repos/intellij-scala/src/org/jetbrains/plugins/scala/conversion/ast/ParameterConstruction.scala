package org.jetbrains.plugins.scala.conversion.ast

/**
  * Created by Kate Ustyuzhanina
  * on 10/27/15
  */
case class ParameterConstruction(modifiers: IntermediateNode,
                                 name: String,
                                 scCompType: IntermediateNode,
                                 isArray: Boolean)
    extends IntermediateNode with TypedElement
  override def getType: TypeConstruction =
    scCompType.asInstanceOf[TypedElement].getType
