package io.gitbub.nicolasfara.rstmanager.ui

/**
 * Pure, field-addressed validation results shared by the forms. A form defines its own `enum Field derives CanEqual` and returns
 * `List[FieldIssue[Field]]`; the page derives the inline error of each field with [[messageFor]] and the flat list (banners, existing specs) with
 * [[messages]].
 */
object FormValidation:

  /** One validation problem of `field`, with its Italian message. */
  final case class FieldIssue[F](field: F, message: String) derives CanEqual

  extension [F](issues: List[FieldIssue[F]])
    /** The first message reported for `field`, if any. */
    def messageFor(field: F)(using CanEqual[F, F]): Option[String] = issues.collectFirst { case issue if issue.field == field => issue.message }

    /** Every message, in order. */
    def messages: List[String] = issues.map(_.message)
end FormValidation
