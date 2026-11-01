package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.RegistryCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.EntityRegistryEvent

class RegistryEventSchemaTest
    extends JournalGoldenSuite[EntityRegistryEvent]("registry-events", RegistryEventSchema, JournalGoldenSuite.caseNames[EntityRegistryEvent]):
  def current: List[String] = List("v1-registered", "v1-deregistered")

  def legacy: List[(String, String)] = List("legacy-v1-registered" -> "v1-registered", "legacy-v1-deregistered" -> "v1-deregistered")
