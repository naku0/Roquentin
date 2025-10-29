package Roquentin.API

import Roquentin.Core.*

object Standard {
  def classes: RuleBuilder = RuleBuilder(AllClassesSelector)
  def noClasses: RuleBuilder = RuleBuilder(AllClassesSelector, isNegated = true)

  def classesThat: SelectorBuilder = SelectorBuilder()
  def noClassesThat: SelectorBuilder = SelectorBuilder(isNegated = true)
}

case class SelectorBuilder(isNegated: Boolean = false) {
  def inPackage(packageName: String): RuleBuilder =
    RuleBuilder(PackageSelector(packageName), isNegated = isNegated)

  def annotatedWith(annotation: String): RuleBuilder =
    RuleBuilder(AnnotatedSelector(annotation), isNegated = isNegated)

  def nameMatching(pattern: String): RuleBuilder =
    RuleBuilder(ClassNameSelector(pattern), isNegated = isNegated)
}

object accessClassesThat {
  def resideIn(packageName: String): ArchitectureCondition = DependsOnCondition(packageName)
}

object beAnnotatedWith {
  def apply(annotation: String): ArchitectureCondition = BeAnnotatedCondition(annotation)
}

object resideInPackage {
  def apply(packageName: String): ArchitectureCondition = ResideInPackageCondition(packageName)
}

object haveNameMatching {
  def apply(pattern: String): ArchitectureCondition = HaveNameMatchingCondition(pattern)
}