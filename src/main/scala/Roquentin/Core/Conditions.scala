package Roquentin.Core

case class DependsOnCondition(targetPackage: String) extends ArchitectureCondition{
  override def check(classes: List[ClassInfo]): ConditionResult = {
    val violations = classes.flatMap{ cls =>
      val illegalDependecies = cls.dependencies.filter(_.startsWith(targetPackage))
      if (illegalDependecies.nonEmpty)
        Some(s"Class ${cls.name} depends on ${illegalDependecies.mkString(", ")}")
      else None
    }
    ConditionResult(violations.isEmpty, violations, classes)
  }

  override def description: String = s"depend on classes n '$targetPackage'"
}

case class BeAnnotatedCondition(annotation: String) extends ArchitectureCondition{
  override def check(classes: List[ClassInfo]): ConditionResult = {
    val violations = classes.filterNot(_.annotations.contains(annotation))
      .map(cls => s"Class '${cls.name}' is not annotated with '@$annotation'")
    ConditionResult(violations.isEmpty, violations, classes)
  }
  override def description: String = s"be annotated with '@$annotation'"
}

case class ResideInPackageCondition(packageName: String) extends ArchitectureCondition{
  override def check(classes: List[ClassInfo]): ConditionResult = {
    val violations = classes.filterNot(_.packageName.startsWith(packageName))
      .map(cls => s"Class ${cls.name} does not reside in package '$packageName'")
    ConditionResult(violations.isEmpty, violations, classes)
  }

  override def description: String = s"reside in package '$packageName'"
}

case class HaveNameMatchingCondition(pattern: String) extends ArchitectureCondition{
  override def check(classes: List[ClassInfo]): ConditionResult = {
    val violations = classes.filterNot(_.name.matches(pattern))
      .map(cls => s"Class '${cls.name}' does not match pattern '$pattern'")
    ConditionResult(violations.isEmpty, violations, classes)
  }

  override def description: String = s"have matching pattern '$pattern'"
}