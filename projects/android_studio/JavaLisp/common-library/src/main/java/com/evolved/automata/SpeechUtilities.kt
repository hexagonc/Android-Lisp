package com.evolved.automata

import kotlin.math.min


public enum class TransformType {INSERTION, SUBSTITUTION, DELETION, MATCH}

public data class MatchResult (val score:Double, val remainingTokens:List<String>, val transformation:List<TransformType>)

class SpeechUtilities {
    companion object {
        fun matchKeywords(phrase1:Collection<String>, phrase2:Collection<String>): Double {


            val group = mutableSetOf<String>()
            var intersectionCount = 0.0

            phrase1.forEach { it ->
                group.add(it)
            }
            var unionCount:Double = group.size.toDouble()

            phrase2.forEach { it ->

                if (group.contains(it)) {
                    intersectionCount++
                } else
                    unionCount++
            }

            return (intersectionCount / unionCount)
        }

        fun wagnerFischerEditDistance(pattern:List<String>, text:List<String>, substitutionCost:Double = 1.0, deletionCost:Double = 1.0, insertionCost:Double = 1.0): Double {
            data class TransformStep (var cost:Double = 0.0, var route:Pair<Int, Int>?=null, var matchP:Boolean = false)

            val N = pattern.size
            val M = text.size

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
                    if (pattern[i-1].equals(text[j-1])){
                        current.matchP = true
                        current.cost = diag.cost
                        route = (i-1) to (j-1)
                    }
                    else {
                        // check substitution route
                        current.matchP = false
                        current.cost = diag.cost + substitutionCost
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

            return matchCount.toDouble()/count.toDouble()
        }

        fun prefixPermutationMatch(pattern:List<String>, text:List<String>, substitutionCost:Double = 1.0, deletionCost:Double = 1.0, insertionCost:Double = 1.0):MatchResult {

            data class TransformStep (var cost:Double = 0.0, var route:Pair<Int, Int>?=null, var matchP:Boolean = false, var transform:TransformType? = null)

            val N = pattern.size
            val M = text.size

            val D = Array(N+1){i->Array(M+1) {j->TransformStep()}}

            // Initialize the top row
            for (j in 0..M) {
                val transform = D[0][j]
                if (j == 0)
                    transform.transform = TransformType.MATCH
                else
                    transform.transform = TransformType.INSERTION
                transform.cost = j.toDouble()
                if (j > 0)
                    transform.route = 0 to (j-1)
                transform.matchP = j == 0
            }

            // Initialize the left column
            for (i in 0..N) {
                val transform = D[i][0]
                if (i == 0)
                    transform.transform = TransformType.MATCH
                else
                    transform.transform = TransformType.DELETION

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
                    if (pattern[i-1].equals(text[j-1])){
                        current.matchP = true
                        current.cost = diag.cost
                        route = (i-1) to (j-1)
                        current.transform = TransformType.MATCH
                    }
                    else {
                        // check substitution route
                        current.matchP = false
                        current.cost = diag.cost + substitutionCost
                        current.transform = TransformType.SUBSTITUTION
                        route = (i-1) to (j-1)

                        // check insertion cost
                        if (left.cost + insertionCost < current.cost){
                            current.cost = left.cost + insertionCost
                            route = i to (j-1)
                            current.transform = TransformType.INSERTION
                        }

                        // check deletion
                        if (top.cost + deletionCost < current.cost) {
                            current.cost = top.cost + deletionCost
                            route = (i-1) to j
                            current.transform = TransformType.DELETION
                        }
                    }
                    current.route = route
                }
            }

            // Get optimal prefix of P
            var count = 0.0

            val transformSteps = mutableListOf<TransformType>()
            var transform:TransformStep = D[N][M]
            var matchCount = 0.0
            val remainingTokens = mutableListOf<String>()
            var lastInsertionP = true
            var pos:Pair<Int, Int>? = N to M
            while (pos!=null){
                val (i, j) = pos
                transform = D[i][j]
                val transformStepType = transform.transform!!

                if (transform.route!=null) // Not first transform type which is a comparison of empty lists
                    transformSteps.add(0, transformStepType)

                if ( lastInsertionP && transformStepType in setOf(TransformType.INSERTION, TransformType.SUBSTITUTION)){
                    remainingTokens.add(0, text[j-1])
                }
                else {
                    lastInsertionP = false
                }
                if (j<=N && j>0) {
                    if (transform.matchP){
                        matchCount++
                    }
                    count++
                }

                pos = transform.route
            }

            val score = matchCount.toDouble()/count.toDouble()
            return MatchResult(score = score, transformation = transformSteps, remainingTokens = remainingTokens)
        }


    }
}