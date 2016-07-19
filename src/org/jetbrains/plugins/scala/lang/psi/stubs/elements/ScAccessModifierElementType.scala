package org.jetbrains.plugins.scala
package lang
package psi
package stubs
package elements

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.{StubElement, StubInputStream, StubOutputStream}
import com.intellij.util.io.StringRef
import org.jetbrains.plugins.scala.lang.psi.api.base.ScAccessModifier
import org.jetbrains.plugins.scala.lang.psi.impl.base.ScAccessModifierImpl
import org.jetbrains.plugins.scala.lang.psi.stubs.impl.ScAccessModifierStubImpl

/**
  * User: Alexander Podkhalyuzin
  * Date: 17.06.2009
  */
class ScAccessModifierElementType[Func <: ScAccessModifier]
  extends ScStubElementType[ScAccessModifierStub, ScAccessModifier]("access modifier") {
  override def serialize(stub: ScAccessModifierStub, dataStream: StubOutputStream): Unit = {
    dataStream.writeBoolean(stub.isProtected)
    dataStream.writeBoolean(stub.isPrivate)
    dataStream.writeBoolean(stub.isThis)
    val hasId = stub.getIdText.isDefined
    dataStream.writeBoolean(hasId)
    if (hasId) {
      dataStream.writeName(stub.getIdText.get)
    }
  }

  override def deserialize(dataStream: StubInputStream, parentStub: StubElement[_ <: PsiElement]): ScAccessModifierStub = {
    val isProtected = dataStream.readBoolean
    val isPrivate = dataStream.readBoolean
    val isThis = dataStream.readBoolean
    val hasId = dataStream.readBoolean
    val idText = if (hasId) Some(dataStream.readName) else None
    new ScAccessModifierStubImpl(parentStub, this, isPrivate, isProtected, isThis, idText)
  }

  override def createStub(psi: ScAccessModifier, parentStub: StubElement[_ <: PsiElement]): ScAccessModifierStub =
    new ScAccessModifierStubImpl(parentStub, this, psi.isPrivate, psi.isProtected,
      psi.isThis, psi.idText.map(StringRef.fromString))

  override def createElement(node: ASTNode): ScAccessModifier = new ScAccessModifierImpl(node)

  override def createPsi(stub: ScAccessModifierStub): ScAccessModifier = new ScAccessModifierImpl(stub)
}