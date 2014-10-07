package chapter1

import org.scalatest.FunSuite

/**
 * Created by jijo.thomas on 10/7/14.
 */
class Recommendations$Test extends FunSuite {

  test("rating for 'Superman Returns' by 'Lisa Rose' should be 3.5") {
    assert(Recommendations.critics("Lisa Rose")("Superman Returns") == 3.5)
  }

  test("Similarity score for 'Lisa Rose' and 'Gene Seymour'") {
    assert(Recommendations.simDistance(Recommendations.critics,"Lisa Rose","Gene Seymour") == 0.29429805508554946)
  }
}
