package Roquentin.Core

import Roquentin.*

case class RuleResult(
                     isSuccess: Boolean,
                     description: String,
                     violations: List[String] = Nil)

case class ClassInfo(
                    name: String,
                    packageName: String,
                    isInterface: Boolean = false,
                    isAbstract: Boolean = false,
                    methods: List[MethodInfo] = Nil,
                    fields: List[FieldInfo] = Nil,
                    dependencies: List[String] = Nil,
                    annotations: List[String] = Nil)

case class MethodInfo(
                     name: String,
                     returnType: String,
                     parameters: List[ParameterInfo],
                     accessModifier: String,
                     annotations: List[String] = Nil)

case class ParameterInfo(
                        name: String,
                        parameterType: String)

case class FieldInfo(
                    name: String,
                    fieldType: String,
                    accessModifier: String,
                    annotations: List[String] = Nil)

trait ArchRule {
  def check(classes: List[ClassInfo]): RuleResult
  def because(reason: String): ArchRule
  def description: String
}

trait ClassSelector {
  def select(classes: List[ClassInfo]) : List[ClassInfo]
  def description: String
}

trait ArchitectureCondition {
  def check(classes: List[ClassInfo]): ConditionResult
  def description: String
}

case class ConditionResult(
                          isOk: Boolean,
                          violations: List[String],
                          checkedClasses: List[ClassInfo] = Nil)

case class NotCondition(condition: ArchitectureCondition) extends ArchitectureCondition {
  override def check(classes: List[ClassInfo]): ConditionResult = {
    val result = condition.check(classes)
    ConditionResult(
      isOk = !result.isOk,
      violations = result.violations,
      checkedClasses = result.checkedClasses
    )
  }
  override def description: String = s"not ${condition.description}"
}

case class AndCondition(conditions: ArchitectureCondition*) extends ArchitectureCondition{
  override def check(classes: List[ClassInfo]): ConditionResult = {
    val results = conditions.map(_.check(classes))
    val allOk = results.forall(_.isOk)
    val allViolations = results.flatMap(_.violations).toList
    val allChecked = results.flatMap(_.checkedClasses).toList
    ConditionResult(allOk, allViolations, allChecked)
  }

  override def description: String = conditions.map(_.description).mkString(" and ")
}

case class OrCondition(conditions: ArchitectureCondition*) extends  ArchitectureCondition{
  override def check(classes: List[ClassInfo]): ConditionResult = {
    val results = conditions.map(_.check(classes))
    val anyOk = results.exists(_.isOk)
    val violations = if (anyOk) Nil else results.flatMap(_.violations).toList
    val checked = results.flatMap(_.checkedClasses).toList

    ConditionResult(anyOk, violations, checked)
  }

  override def description: String = conditions.map(_.description).mkString(" or ")
}

case class RuleBuilder(
                        selector: ClassSelector,
                        condition: Option[ArchitectureCondition] = None,
                        description: String = "",
                        isNegated: Boolean = false) {

  def because(reason:String) : RuleBuilder =
    copy(description = reason)

  def should(condition: ArchitectureCondition) : RuleBuilder =
    copy(condition = Some(condition))

  def shouldNot(condition: ArchitectureCondition) : RuleBuilder =
    copy(condition = Some(NotCondition(condition)))

  def build: ArchRule = {
    require(condition.isDefined, "Condition must be pprovided before building rule")

    val finalCondition = if (isNegated) NotCondition(condition.get) else condition.get
    
    new ArchRule {
      override def check(classes: List[ClassInfo]): RuleResult = {
        val selectedClasses = selector.select(classes)
        val conditionResult = finalCondition.check(selectedClasses)
        
        RuleResult(
          isSuccess = conditionResult.isOk,
          description = if (description.nonEmpty) description
                        else s"${selector.description} ${finalCondition.description}",
          violations = conditionResult.violations
        )
      }

      override def because(reason: String): ArchRule = 
        RuleBuilder(selector, condition, reason, isNegated).build

      override def description: String = if (description.nonEmpty) description
                                         else s"${selector.description} ${finalCondition.description}"
    }
  }
}

object ArchRule {
  def classes(selector: ClassSelector): RuleBuilder = RuleBuilder(selector)
}
