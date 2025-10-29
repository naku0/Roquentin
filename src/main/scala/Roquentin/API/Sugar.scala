package Roquentin.API

import Roquentin.Core._

object Sugar {
  import Roquentin.API._

  def classes: RuleBuilder = Standard.classes
  def noClasses: RuleBuilder = Standard.noClasses
  def classesThat: SelectorBuilder = Standard.classesThat
  def noClassesThat: SelectorBuilder = Standard.noClassesThat

  implicit class SelectorBuilderInfix(builder: SelectorBuilder) {
    def inPackage(packageName: String): RuleBuilder = builder.inPackage(packageName)
    def annotatedWith(annotation: String): RuleBuilder = builder.annotatedWith(annotation)
    def nameMatching(pattern: String): RuleBuilder = builder.nameMatching(pattern)
  }

  implicit class RuleBuilderInfix(builder: RuleBuilder) {
    def should(condition: ArchitectureCondition): RuleBuilder = builder.should(condition)
    def because(reason: String): RuleBuilder = builder.because(reason)
  }
}