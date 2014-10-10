import chapter1.Recommendations
import chapter1.Recommendations.Preferences


val c:Map[String,Map[String,Double]] = Recommendations.critics

val movies = Recommendations.transformPrefs(Recommendations.critics)

val matches = Recommendations.topMatches(movies,"Superman Returns")

val reviewers = Recommendations.getRecommendations(movies,"Just My Luck")