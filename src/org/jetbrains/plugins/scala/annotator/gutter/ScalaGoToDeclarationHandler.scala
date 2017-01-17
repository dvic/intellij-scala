package org.jetbrains.plugins.scala
package annotator
package gutter

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.psi._
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScAssignStmt, ScReferenceExpression, ScSelfInvocation}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.resolve.processor.DynamicResolveProcessor
import org.jetbrains.plugins.scala.lang.resolve.{ResolvableReferenceElement, ScalaResolveResult}

/**
  * User: Alexander Podkhalyuzin
  * Date: 22.11.2008
  */
class ScalaGoToDeclarationHandler extends GotoDeclarationHandler {

  def getGotoDeclarationTargets(sourceElement: PsiElement, offset: Int, editor: Editor): Array[PsiElement] = {
    if (sourceElement == null) return null

    val containingFile = sourceElement.getContainingFile
    if (containingFile == null) return null

    val element = containingFile.findElementAt(offset)
    if (element == null) return null
    if (!element.getLanguage.isKindOf(ScalaLanguage.INSTANCE)) return null

    import ScalaGoToDeclarationHandler._
    element.getNode.getElementType match {
      case ScalaTokenTypes.tASSIGN => assignmentCase(element.getParent)
      case ScalaTokenTypes.kTHIS => thisCase(element.getParent)
      case ScalaTokenTypes.tIDENTIFIER =>
        val reference = containingFile.findReferenceAt(element.getTextRange.getStartOffset)
        identifierCase(reference)
      case _ => null
    }
  }

  def getActionText(context: DataContext): String = null
}

object ScalaGoToDeclarationHandler {
  private def assignmentCase(element: PsiElement) = element match {
    case statement: ScAssignStmt =>
      Option(statement.assignNavigationElement).toArray
    case _ => null
  }


  private def thisCase(element: PsiElement) = element match {
    case invocation: ScSelfInvocation =>
      invocation.bind match {
        case Some(bind) => Array(bind)
        case _ => null
      }
    case _ => null
  }

  private def identifierCase(reference: PsiReference): Array[PsiElement] = {
    val elements = reference match {
      case null => null
      case expression: ScReferenceExpression if DynamicResolveProcessor.isDynamicReference(expression) =>
        new DynamicResolveProcessor(expression).resolve()
          .distinct
          .flatMap(adaptDynamic)
      case resRef: ResolvableReferenceElement =>
        resRef.bind() match {
          case Some(bind) =>
            Seq(bind)
              .flatMap(adaptScala)
              .distinct
          case _ => null
        }
      case r => Option(r.resolve()).toSeq
    }

    elements match {
      case null => null
      case _ => elements.map(targetElement).toArray
    }
  }

  private[this] def targetElement(element: PsiElement): PsiElement = {
    val maybeResult = element match {
      case function: ScFunction => function.getSyntheticNavigationElement
      case definition: ScTypeDefinition if definition.isSynthetic => definition.syntheticContainingClass
      case obj: ScObject if obj.isSyntheticObject => ScalaPsiUtil.getCompanionModule(obj)
      case parameter: ScParameter => ScalaPsiUtil.parameterForSyntheticParameter(parameter)
      case _ => None
    }

    maybeResult.getOrElse(element)
  }

  private[this] def adaptScala(resolveResult: ScalaResolveResult): Seq[PsiElement] =
    (resolveResult.element, resolveResult.getActualElement) match {
      case (function: ScFunction, actualElement) if function.isSynthetic =>
        Seq(actualElement)
      case (method: PsiMethod, actualElement) if method.isConstructor && method.containingClass == actualElement =>
        Seq(method)
      case (resolved, actualElement) =>
        Seq(actualElement, resolved) ++ resolveResult.innerResolveResult.map(_.getElement)
    }

  private[this] def adaptDynamic(resolveResult: ResolveResult): Option[PsiElement] =
    Option(resolveResult.getElement)
}