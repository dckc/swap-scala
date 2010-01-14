package org.w3.swap.test

import org.w3.swap
import swap.logic.Formula

class TestSuite(val manifest: Formula) {
}

class EntailmentTestSuite(override val manifest: Formula)
  extends TestSuite(manifest) {
}
