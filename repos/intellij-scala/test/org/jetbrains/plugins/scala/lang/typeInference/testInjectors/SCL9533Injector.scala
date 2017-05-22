package org.jetbrains.plugins.scala.lang.typeInference.testInjectors

import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector

/**
  * @author Alefas
  * @since 16/02/16
  */
class SCL9533Injector extends SyntheticMembersInjector
  override def needsCompanionObject(source: ScTypeDefinition): Boolean =
    source.findAnnotation("something.enhance") != null
