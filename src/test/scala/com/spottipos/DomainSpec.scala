package com.spottipos

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import com.spotippos.ApotipposWorld
import com.spotippos.Gode
import com.spotippos.Groola
import com.spotippos.Jaby
import com.spotippos.SimpleApotipposWorld
import com.spotippos.Nova
import com.spotippos.Ruja
import com.spotippos.Scavy
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DomainSpec extends Specification {
  val spec = this

  "ApotipposWorld" >> {
    "Contains Gode, Ruja, Jaby, Scavy, Groola and Nova provinces" >> {
      ApotipposWorld.registry.land(0)(0).province.size === 1
      ApotipposWorld.registry.land(0)(0).province(0) === Scavy

      ApotipposWorld.registry.land(1400)(1000).province.size === 1
      ApotipposWorld.registry.land(1400)(1000).province(0) === Jaby

      ApotipposWorld.registry.land(0)(1000).province.size === 1
      ApotipposWorld.registry.land(0)(1000).province(0) === Gode

      ApotipposWorld.registry.land(700)(1000).province.size === 1
      ApotipposWorld.registry.land(700)(1000).province(0) === Ruja

      ApotipposWorld.registry.land(700)(0).province.size === 1
      ApotipposWorld.registry.land(700)(0).province(0) === Groola

      ApotipposWorld.registry.land(1400)(0).province.size === 1
      ApotipposWorld.registry.land(1400)(0).province(0) === Nova

      ApotipposWorld.registry.land(450)(1000).province.size === 2
      ApotipposWorld.registry.land(450)(1000).province(0) === Gode
      ApotipposWorld.registry.land(450)(1000).province(1) === Ruja
    }
  }

  "SimpleApotipposWorld" >> {
    "Contains Gode, Ruja, Jaby, Scavy, Groola and Nova provinces" >> {
      SimpleApotipposWorld.registry.land(0)(0).province.size === 1
      SimpleApotipposWorld.registry.land(0)(0).province(0) === Scavy

      SimpleApotipposWorld.registry.land(1400)(1000).province.size === 1
      SimpleApotipposWorld.registry.land(1400)(1000).province(0) === Jaby

      SimpleApotipposWorld.registry.land(0)(1000).province.size === 1
      SimpleApotipposWorld.registry.land(0)(1000).province(0) === Gode

      SimpleApotipposWorld.registry.land(700)(1000).province.size === 1
      SimpleApotipposWorld.registry.land(700)(1000).province(0) === Ruja

      SimpleApotipposWorld.registry.land(700)(0).province.size === 1
      SimpleApotipposWorld.registry.land(700)(0).province(0) === Groola

      SimpleApotipposWorld.registry.land(1400)(0).province.size === 1
      SimpleApotipposWorld.registry.land(1400)(0).province(0) === Nova

      SimpleApotipposWorld.registry.land(450)(1000).province.size === 2
      SimpleApotipposWorld.registry.land(450)(1000).province(0) === Gode
      SimpleApotipposWorld.registry.land(450)(1000).province(1) === Ruja
    }
  }

}