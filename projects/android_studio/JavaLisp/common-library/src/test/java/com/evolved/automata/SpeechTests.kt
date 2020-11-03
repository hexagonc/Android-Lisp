package com.evolved.automata
import org.junit.Assert.assertTrue
import org.junit.Test
import kotlin.math.min


class SpeechTests {

    @Test
    fun testKeywordMatching() {

        val keys = setOf("yes")

        val speech = setOf("yes")

        val expectedMatchFraction = 1.0F

        val union = mutableSetOf<String>()
        var intersectionCount = 0

        keys.forEach { it ->
            union.add(it)
        }
        var unionCount = union.size

        speech.forEach { it ->

            if (union.contains(it)) {
                intersectionCount++
            } else
                unionCount++
        }

        val matchFraction = (intersectionCount / unionCount).toFloat()
        assertTrue(matchFraction == expectedMatchFraction)
    }

    @Test
    fun testPermutationMatching(){

        val substitionCost = 1.0
        val deletionCost = 1.0
        val insertionCost = 1.0
        val P = arrayOf("my", "name", "is")
        //val T = arrayOf("my", "name", "is", "andrew")
        val T = arrayOf("my", "name", "is")

        data class TransformStep (var cost:Double = 0.0, var route:Pair<Int, Int>?=null, var matchP:Boolean = false)

        val N = P.size
        val M = T.size

        val D = Array(N+1){i->Array(M+1) {j->TransformStep()}}

        // Initialize the top row
        for (j in 0..M) {
            val transform = D[0][j]
            transform.cost = j.toDouble()
            if (j > 0)
                transform.route = 0 to (j-1)
            transform.matchP = j == 0
        }

        // Initialize the left column
        for (i in 0..N) {
            val transform = D[i][0]
            transform.cost = i.toDouble()
            if (i > 0)
                transform.route = 0 to (i-1)
            transform.matchP = i == 0
        }

        for (i in 1..N){
            for (j in 1..M){
                val diag = D[i-1][j-1]
                val top = D[i-1][j]
                val left = D[i][j-1]

                val current = D[i][j]

                var route:Pair<Int, Int>? = null
                if (P[i-1].equals(T[j-1])){
                    current.matchP = true
                    current.cost = diag.cost
                    route = (i-1) to (j-1)
                }
                else {
                    // check substitution route
                    current.matchP = false
                    current.cost = diag.cost + substitionCost
                    route = (i-1) to (j-1)

                    // check insertion cost
                    if (left.cost + insertionCost < current.cost){
                        current.cost = left.cost + insertionCost
                        route = i to (j-1)
                    }

                    // check deletion
                    if (top.cost + deletionCost < current.cost) {
                        current.cost = top.cost + deletionCost
                        route = (i-1) to j
                    }
                }
                current.route = route
            }
        }

        // Get full score
        var count = 0
        var transform:TransformStep? = D[N][M]
        var matchCount = 0
        while (transform!=null){
            if (transform.matchP){
                matchCount++
            }
            count++
            val prev = transform.route
            if (prev != null){

                transform = D[prev.first][prev.second]
            }
            else {
                transform = null
            }

        }

        val score = matchCount.toDouble()/count.toDouble()

        val expectedScore = 1.0
        assertTrue("Expected perfect match score", score == expectedScore)

    }


    @Test
    fun testPrefixPermutationMatching(){


        val substitionCost = 1.0
        val deletionCost = 1.0
        val insertionCost = 1.0
        val P = arrayOf("my", "name", "is")
        //val T = arrayOf("my", "name", "is", "andrew")
        val T = arrayOf("my", "name", "is", "Andrew")

        data class TransformStep (var cost:Double = 0.0, var route:Pair<Int, Int>?=null, var matchP:Boolean = false)

        val N = P.size
        val M = T.size

        val D = Array(N+1){i->Array(M+1) {j->TransformStep()}}

        // Initialize the top row
        for (j in 0..M) {
            val transform = D[0][j]
            transform.cost = j.toDouble()
            if (j > 0)
                transform.route = 0 to (j-1)
            transform.matchP = j == 0
        }

        // Initialize the left column
        for (i in 0..N) {
            val transform = D[i][0]
            transform.cost = i.toDouble()
            if (i > 0)
                transform.route = 0 to (i-1)
            transform.matchP = i == 0
        }


        for (i in 1..N){
            for (j in 1..M){
                val diag = D[i-1][j-1]
                val top = D[i-1][j]
                val left = D[i][j-1]

                val current = D[i][j]

                var route:Pair<Int, Int>? = null
                if (P[i-1].equals(T[j-1])){
                    current.matchP = true
                    current.cost = diag.cost
                    route = (i-1) to (j-1)
                }
                else {
                    // check substitution route
                    current.matchP = false
                    current.cost = diag.cost + substitionCost
                    route = (i-1) to (j-1)

                    // check insertion cost
                    if (left.cost + insertionCost < current.cost){
                        current.cost = left.cost + insertionCost
                        route = i to (j-1)
                    }

                    // check deletion
                    if (top.cost + deletionCost < current.cost) {
                        current.cost = top.cost + deletionCost
                        route = (i-1) to j
                    }
                }
                current.route = route
            }
        }

        // Get optimal prefix of P
        var count = 0

//        var start:TransformStep? = null
//        for (i in 0..N) {
//            if (start == null || start.cost < D[i][min(N, M)].cost){
//
//            }
//        }
        var transform:TransformStep? = D[N][min(N, M)]
        var matchCount = 0
        while (transform!=null){
            if (transform.matchP){
                matchCount++
            }
            count++
            val prev = transform.route
            if (prev != null){

                transform = D[prev.first][prev.second]
            }
            else {
                transform = null
            }

        }

        val score = matchCount.toDouble()/count.toDouble()

        val expectedScore = 1.0
        assertTrue("Expected perfect prefix match score", score == expectedScore)

    }

    @Test
    fun testPrefixPermutationMatching2(){

        val P = listOf("her","name", "is")

        val T = listOf("Andrew", "Harrell", "Baughns")

        val result:MatchResult = SpeechUtilities.prefixPermutationMatch(P, T)

        print("Output: $result")

    }
}