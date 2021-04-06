package com.evolved.automata

import com.evolved.automata.lisp.Value
import org.junit.Assert.assertThat
import org.junit.Assert.assertTrue
import org.junit.Test
import kotlin.math.abs
import kotlin.math.max

typealias Chain = List<String>


class WorldLineSpeechTests {

    data class MatchStats(val matchOrderSets:List<Map<String, Int>>, var orderScore: Float?, var setScore:Float? = null)
    data class ClustSpec (val phrase:String, val name:String, var synonmys:MutableMap<String, String> = mutableMapOf(), var stats: MatchStats?=null)
    data class matchTokenClusterCache (var index:MutableMap<String, MutableSet<String>>, val matchStats:MutableMap<String, Double>, val handlerMap:Map<String, ClustSpec>)



    // **** keep ******
    /**
     * Returns a list of all intent-names that matched the token in the corresponding position in the original
     * input phrase
     */
    fun buildMatchList(phrase:String, index:Map<String, Set<String>>,  includeMin:Boolean = true, includeMax:Boolean = true, includeAll: Boolean = true ):List<Set<String>> {
        return buildMatchList(phrase.split(' '), index, includeMin, includeMax, includeAll)
    }

    fun buildMatchList(phraseTokens:List<String>, index:Map<String, Set<String>>,  includeMin:Boolean = true, includeMax:Boolean = true, includeAll: Boolean = true ):List<Set<String>> {
        // TODO: Rename this
        fun MutableMap<String, Int>.incrMap(key: String, defaultValue:Int = 0, weight: Int = 1) : MutableMap<String, Int> {
            if (containsKey(key)){
                put(key, weight + get(key)!!)
            }
            else {
                put(key, defaultValue + weight)
            }
            return this
        }

        val out = mutableListOf<Set<String>>()
        var matchMap = mutableMapOf<String, Int>()
        var prevSet:Set<String>? = null

        for (token in phraseTokens) {
            val matchSet:Set<String>? = index[token]

            if (matchSet != null){
                var minKeys = mutableMapOf<String, Int>()
                var maxKeys = mutableMapOf<String, Int>()

                for (clusterName in matchSet){
                    matchMap.incrMap(clusterName)

                    if (minKeys.isEmpty() || (minKeys.iterator().next().value > matchMap[clusterName]!!)){
                        minKeys[clusterName] = matchMap[clusterName]!!
                    }
                    else if (minKeys.iterator().next().value == matchMap[clusterName]!!){
                        minKeys[clusterName] =  matchMap[clusterName]!!
                    }


                    if (maxKeys.isEmpty() || (maxKeys.iterator().next().value < matchMap[clusterName]!!)){
                        maxKeys[clusterName] = matchMap[clusterName]!!
                    }
                    else if (maxKeys.iterator().next().value == matchMap[clusterName]!!){
                        maxKeys[clusterName] =  matchMap[clusterName]!!
                    }
                }

                for (clusterName in matchSet) {
                    if (includeAll)
                        continue

                    if (includeMax && maxKeys.containsKey(clusterName)){
                        continue
                    }

                    if (includeMin && minKeys.containsKey(clusterName))
                        continue

                    matchMap.remove(clusterName)
                }

                prevSet?.forEach{priorClusterName ->
                    if (!matchSet.contains(priorClusterName)) {
                        matchMap.remove(priorClusterName)
                    }
                }

                prevSet = matchSet
            }
            else {
                prevSet = null
                matchMap = mutableMapOf<String, Int>()
            }

            out.add(matchMap.keys.toSet())
        }
        return out

    }

    fun makeCluster(clusterName:String, canonicalPhrase:String, synonymMap:Map<String, String>? = null): ClustSpec {
        val tokens = canonicalPhrase.split(' ')
        var synonyms = mutableMapOf<String, String>(*tokens.map{token -> token to token}.toTypedArray())
        if (synonymMap != null) {
            synonyms.putAll(synonymMap)
        }
        return ClustSpec(canonicalPhrase, clusterName, synonyms)
    }

    fun buildIndex(handlers:List<ClustSpec>): MutableMap<String, MutableSet<String>> {
        fun addHandler(handler: ClustSpec, invertedIndex:MutableMap<String, MutableSet<String>>){
            handler.synonmys.forEach{(spokenWord:String, _:String)->
                var prior = invertedIndex?.get(spokenWord)?: mutableSetOf()
                prior.add(handler.name)
                invertedIndex[spokenWord] = prior
            }
        }

        val invertedIndex = mutableMapOf<String, MutableSet<String>>()

        for (spec:ClustSpec in handlers){
            addHandler(spec, invertedIndex)
        }
        return invertedIndex
    }

    fun filterViableClusters(tokenClusters:List<ClustSpec>, phrase: String): Set<String> {
        val out = mutableSetOf<String>()

        val words = phrase.split(' ')
        val index = buildIndex(tokenClusters)

        for (word in words){
            val candidates = index[word]
            if (candidates != null){
                candidates.forEach {clusterName: String -> out.add(clusterName)}
            }
        }
        return out.toSet()
    }

    fun matchTokenCluster(tokenClusters:List<ClustSpec>, phrase:String): Set<String> {
        val words = phrase.split(' ')

        var maxScore:Float? = null
        var maxClusters:MutableSet<String>? = null

        if (phrase.trim().length == 0)
            return setOf<String>()
        else {
            val environment = matchTokenClusterCache(mutableMapOf(), mutableMapOf(), mapOf<String, ClustSpec>(*(tokenClusters.map {spec -> spec.name to spec}).toTypedArray()))

            environment.index = buildIndex(tokenClusters)

            val viableClusterNames = filterViableClusters(tokenClusters, phrase)

            for (clusterName in viableClusterNames){
                val cluster = environment.handlerMap[clusterName]!!
                val tokens = cluster.phrase.split(' ')

                var score = 0.0F
                if (cluster.stats == null){
                    val matchOrderSpec = buildTokenOrderMapsList(tokens, 3)
                    score = getMatchScore(matchOrderSpec, words)
                    val stats = MatchStats(matchOrderSpec, score)
                    stats.orderScore = score
                    cluster.stats = stats
                }
                else
                    score = cluster?.stats?.orderScore!!

                if (score > 0){
                    if (maxScore == null || (maxScore < score)){
                        maxClusters = mutableSetOf(clusterName)
                        maxScore = score
                    }
                    else if (maxScore == score) {
                        maxClusters?.add(clusterName)
                    }
                }
            }

            if (maxClusters != null)
                return maxClusters.toSet()
            else
                return setOf<String>()

            //TODO("${Thread.currentThread().stackTrace[1].methodName} (${Thread.currentThread().stackTrace[0].lineNumber}) Not implemented")
        }

    }

    // ***************************************************

    val objectRepo = FiniteSet(500)

    val SPEECH_PROCESSING_COGJECT_NAME = "speech-processor"



    @Test fun simpleSpeechTests(){
        var phrase = "andrew move forward please for 10 seconds"

        val worldline = WorldLine()

        val handlers = listOf<IntentHandler>(
                makeSpeechIntent(
                        intentTokenName = "[forward]",
                         phrase = "forward",
                        synonymMap = mutableMapOf("go" to "forward"),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                          nluWorld.addValueCogject(intentName, "moving forward", time)
                            println("Moving forward")
                            HANDLER_STATE.SUCCESS}),
                makeSpeechIntent(
                        intentTokenName = "[number]",
                        phrase = "number",
                        synonymMap = mutableMapOf(*listOf(0, 1, 2, 3, 4,5,6,7,8,9, 10).map {n -> n.toString() to "number"}.toTypedArray()),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                            val numValue = Integer.parseInt(tokens[0])
                            nluWorld.addValueCogject(intentName, numValue, time)
                            println("Ran for ${numValue} minutes")
                            HANDLER_STATE.SUCCESS}),
                makeSpeechIntent(
                        intentTokenName = "[forward-duration]",
                        phrase = "move forward for [number] seconds",
                        synonymMap = mutableMapOf("go" to "move"),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                            val duration = nluWorld.getCogjectValue<Int>("[number]", time)
                            nluWorld.addValueCogject(intentName, duration!!, time)
                            println("Finished moving forward by ${duration} seconds")
                            HANDLER_STATE.SUCCESS}),
                makeSpeechIntent(
                        intentTokenName = "[backward]",
                        phrase = "backward",
                        synonymMap = mutableMapOf("back" to "backward", "reverse" to "backward"),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                            nluWorld.addValueCogject(intentName, "moving backward", time)
                            println("Moving back")
                            HANDLER_STATE.SUCCESS})

        )


        processSpeech(phrase, handlers, worldline)

        assertTrue("Expected match for ${phrase}", worldline.hasValue("[forward-duration]"))
    }

    @Test fun mediumSpeechTests(){
        var phrase = "when greater than 6 then go"

        val worldline = WorldLine()

        val handlers = listOf<IntentHandler>(
                makeSpeechIntent(
                        intentTokenName = "[less-than-5]",
                        phrase = "less than 5",
                        basePredicates = setOf("<predicate>"),
                        synonymMap = mutableMapOf("five" to "5"),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                            nluWorld.addValueCogject(intentName, "< 5", time)
                            nluWorld.addValueCogject("predicate-type", "less-than", time)
                            nluWorld.addValueCogject("less-than-value", 5, time)
                            HANDLER_STATE.SUCCESS}),
                makeSpeechIntent(
                        intentTokenName = "[greater-than-6]",
                        phrase = "greater than 6",
                        synonymMap = mutableMapOf("size" to "6"),
                        basePredicates = setOf("<predicate>"),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                            nluWorld.addValueCogject(intentName, "> 6", time)
                            nluWorld.addValueCogject("predicate-type", "greater-than", time)
                            nluWorld.addValueCogject("greater-than-value", 6, time)
                            HANDLER_STATE.SUCCESS}),
                makeSpeechIntent(
                        intentTokenName = "[conditional]",
                        phrase = "when <predicate> then go forward",
                        synonymMap = mutableMapOf("if" to "when", "move" to "go"),
                        requiredCogjects = setOf("predicate-type"),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->

                            nluWorld.addValueCogject(intentName, "Predicate became true", time)
                            val predType = nluWorld.getCogjectValue<String>("predicate-type", time)
                            var predMetaData: Any? = null
                            when (predType) {
                                "less-than" ->  predMetaData = nluWorld.getCogjectValue<Int>("less-than-value", time)
                                "greater-than" -> predMetaData = nluWorld.getCogjectValue<Int>("greater-than-value", time)
                                else ->  print("Unknown predicate type")
                            }

                            println("Conditional match with predicate type: ${predType} when meta-data: ${predMetaData?:"nothing"}")

                            HANDLER_STATE.SUCCESS}),
                makeSpeechIntent(
                        intentTokenName = "[number]",
                        phrase = "number",
                        synonymMap = mutableMapOf(*listOf(0, 1, 2, 3, 4,5,6,7,8,9, 10).map {n -> n.toString() to "number"}.toTypedArray()),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                            val numValue = Integer.parseInt(tokens[0])
                            nluWorld.addValueCogject(intentName, numValue, time)
                            println("Ran for ${numValue} minutes")
                            HANDLER_STATE.SUCCESS}),
                makeSpeechIntent(
                        intentTokenName = "[forward-duration]",
                        phrase = "move forward for [number] seconds",
                        synonymMap = mutableMapOf("go" to "move"),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                            val duration = nluWorld.getCogjectValue<Int>("[number]", time)
                            nluWorld.addValueCogject(intentName, duration!!, time)
                            if (tokens.size > 3){
                                println("Finished moving forward by ${duration} seconds")
                                HANDLER_STATE.SUCCESS
                            }
                            else {
                                println("Insufficient arguments to process ${intentName}")
                                HANDLER_STATE.FAILURE
                            }

                        }),
                makeSpeechIntent(
                        intentTokenName = "[backward]",
                        phrase = "backward",
                        synonymMap = mutableMapOf("back" to "backward", "reverse" to "backward"),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                            nluWorld.addValueCogject(intentName, "moving backward", time)
                            println("Moving back")
                            HANDLER_STATE.SUCCESS})

        )


        processSpeech(phrase, handlers, worldline)

        assertTrue("Expected match for ${phrase}", worldline.hasValue("[conditional]"))
    }

    @Test fun edgeCaseSpeechTests(){
        var phrase = "my name is Andrew"

        val worldline = WorldLine()

        val handlers = listOf<IntentHandler>(
                makeSpeechIntent(
                        intentTokenName = "[user-says-name]",
                        phrase = "my name is [name]",
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->

                            val username = nluWorld.getCogjectValue<String>("[name]", time)
                            println("Hello, ${username}")
                            nluWorld.addValueCogject(intentName, username!!, time)
                            HANDLER_STATE.SUCCESS}),
                makeSpeechIntent(
                        intentTokenName = "[name]",
                        phrase = "name",
                        synonymMap = mutableMapOf("Andrew" to "name", "Aprill" to "name", "Paul" to "name", "Rindi" to "name"),
                        handler = {tokens:List<String>, nluWorld:WorldLine, time:Long ->
                            val spokenName = tokens.find { t -> synonoyms.containsKey(t) && t != "name"}
                            if (spokenName != null){
                                nluWorld.addValueCogject(intentName, spokenName, time)
                                HANDLER_STATE.SUCCESS
                            }
                            else {
                                HANDLER_STATE.FAILURE
                            }})

        )


        processSpeech(phrase, handlers, worldline)

        assertTrue("Expected match for ${phrase}", worldline.hasValue("[user-says-name]"))
    }


    @Test fun developBuildChains(){
        val clusters = listOf<ClustSpec>(
                makeCluster("move-forward-centi", "move forward by 10 centimeters", mapOf("for" to "by", "go" to "move", "ten" to "10")),
                makeCluster("do-something-for-duration-centi", "by 10 centimeters", mapOf("for" to "by", "ten" to "10")),
                makeCluster("number-10", "10", mapOf("ten" to "10")),

                makeCluster("move-backward-centi", "move backward by 10 centimeters", mapOf("for" to "by", "go" to "move", "ten" to "10")),
                makeCluster("move-backward", "move backward", mapOf("for" to "by", "go" to "move")),
                makeCluster("move-forward", "move forward", mapOf("for" to "by", "go" to "move")),


                makeCluster("rotate-left-seconds", "rotate left for 2 seconds", mapOf("turn" to "rotate", "two" to "2")),
                makeCluster("rotate-left", "rotate left", mapOf("turn" to "rotate")),
                makeCluster("do-something-for-duration-seconds", "for 2 seconds", mapOf("two" to "2")),
                makeCluster("duration-seconds", "2 seconds", mapOf("two" to "2")),
                makeCluster("number-2", "2", mapOf("two" to "2")),

                makeCluster("rotate-right-seconds", "rotate right for 2 seconds", mapOf("turn" to "rotate", "two" to "2")),
                makeCluster("rotate-right", "rotate right", mapOf("turn" to "rotate")),

                makeCluster("rotate-right-degrees", "rotate right for 45 degrees", mapOf("for" to "by", "turn" to "rotate")),
                makeCluster("do-something-over-degrees-duration", "for 45 degrees", mapOf("for" to "by", "forty-five" to "45")),
                makeCluster("degrees-duration", "45 degrees", mapOf("forty-five" to "45")),
                makeCluster("number-45", "45", mapOf("forty-five" to "45"))
        )

        val includeAll = false
        val includeMax = true
        val includeMin = true

        var phrase = "rotate left for 2 seconds"
        var phraseTokens = phrase.split(' ')

        var index = buildIndex(clusters)

        val partition: List<Set<String>> = buildMatchList(phrase, index, includeMin, includeMax, includeAll)
        for ((i, matchSet) in partition.withIndex()){
            val token = phraseTokens[i]
            println("matching ${token} -> ${matchSet.sorted()}")
        }

        val partitionName = partition[0].iterator().next()

        val expectedChain = listOf<String>("rotate-left", "rotate-left")

        fun getAPartition(partition: List<Set<String>>, segmentName:String, offset: Int = 0): List<String>? {

            var linkName = segmentName
            var branch = mutableListOf<String>()
            outer@for (i in offset until partition.size){
                if (partition[i].contains(linkName)){
                    branch.add(linkName)
                }
                else if (i > offset){
                    var maxSet = mutableMapOf<String, Int>()
                    var remains = mutableListOf<List<String>>()
                    for (name in partition[i]){

                        if (!partition[i-1].contains(name)){
                            val remaining = getAPartition(partition, name, i)
                            if (remaining != null) {
                                var prefixLength = remaining.takeWhile { it == name }.size

                                if (maxSet.isEmpty() ||
                                        maxSet.iterator().next().value < prefixLength){
                                    maxSet = mutableMapOf(name to prefixLength)
                                    remains = mutableListOf<List<String>>(remaining)
                                }
                                else if (maxSet.iterator().next().value == prefixLength){
                                    maxSet[name] = prefixLength
                                    remains.add(remaining)
                                }
                            }

                        }
                    }
                    if (branch.isNotEmpty() && remains.isNotEmpty()){
                        return branch.apply {addAll(remains.random())}.toList()
                    }
                    return null
                }
            }

            return branch
        }

        val out = getAPartition(partition, "rotate-left")

        println(out)

    }

    data class PositionalMatchList(val positionalMatches: List<Set<String>>, var matchChains: List<Chain>? = null)


    @Test fun test_word_partitioning2(){
        val clusters = listOf<ClustSpec>(
                makeCluster("move-forward-centi", "move forward by 10 centimeters", mapOf("for" to "by", "go" to "move", "ten" to "10")),
                makeCluster("do-something-for-duration-centi", "by 10 centimeters", mapOf("for" to "by", "ten" to "10")),
                makeCluster("number-10", "10", mapOf("ten" to "10")),

                makeCluster("move-backward-centi", "move backward by 10 centimeters", mapOf("for" to "by", "go" to "move", "ten" to "10")),
                makeCluster("move-backward", "move backward", mapOf("for" to "by", "go" to "move")),
                makeCluster("move-forward", "move forward", mapOf("for" to "by", "go" to "move")),


                makeCluster("rotate-left-seconds", "rotate left for 2 seconds", mapOf("turn" to "rotate", "two" to "2")),
                makeCluster("rotate-left", "rotate left", mapOf("turn" to "rotate")),
                makeCluster("do-something-for-duration-seconds", "for 2 seconds", mapOf("two" to "2")),
                makeCluster("duration-seconds", "2 seconds", mapOf("two" to "2")),
                makeCluster("number-2", "2", mapOf("two" to "2")),

                makeCluster("rotate-right-seconds", "rotate right for 2 seconds", mapOf("turn" to "rotate", "two" to "2")),
                makeCluster("rotate-right", "rotate right", mapOf("turn" to "rotate")),

                makeCluster("rotate-right-degrees", "rotate right for 45 degrees", mapOf("for" to "by", "turn" to "rotate")),
                makeCluster("do-something-over-degrees-duration", "for 45 degrees", mapOf("for" to "by", "forty-five" to "45")),
                makeCluster("degrees-duration", "45 degrees", mapOf("forty-five" to "45")),
                makeCluster("number-45", "45", mapOf("forty-five" to "45"))
        )

        val includeAll = false
        val includeMax = true
        val includeMin = true

        var phrase = "rotate left for 2 seconds"
        var phraseTokens = phrase.split(' ')

        var index = buildIndex(clusters)

        val partition: List<Set<String>> = buildMatchList(phrase, index, includeMin, includeMax, includeAll)
        for ((i, matchSet) in partition.withIndex()){
            val token = phraseTokens[i]
            println("matching ${token} -> ${matchSet.sorted()}")
        }
    }

    @Test fun test_simple_hierarchical_1(){
        val clusters = listOf(makeCluster("rotate-left-seconds", "rotate left do-something-for-duration-seconds", mapOf("turn" to "rotate", "two" to "2")),
                makeCluster("rotate-left", "rotate left", mapOf("turn" to "rotate")),
                makeCluster("do-something-for-duration-seconds", "for duration-seconds", mapOf("two" to "2")),
                makeCluster("duration-seconds", "(number) seconds", mapOf( *((1..10).map {i -> "$i" to "(number)"}.toTypedArray()))),
                makeCluster("number-2", "2", mapOf("two" to "2")))

        val totalIndex = buildIndex(clusters)

        val handlerIndex = mapOf<String, ClustSpec>(*clusters.map {spec -> spec.name to spec}.toTypedArray())

        val base_phrase = "rotate left for 3 seconds"

        val token_map = base_phrase.split(' ').map {word -> mutableSetOf(word)}

        fun process_token_map(tokens: List<MutableSet<String>>): List<MutableSet<String>> {
            return tokens.map { tokenSet ->
                var out = mutableSetOf<String>();
                tokenSet.forEach{token -> out.add(token); if (totalIndex[token]!=null) out.addAll(totalIndex[token]!!)}

                out}
        }

        fun expand_token_set(tokenSet: Set<String>, maxLevels:Int = 100): MutableSet<String> {
            var set = mutableSetOf<String>().apply {addAll(tokenSet)}
            var next:MutableSet<String> = set
            var level = 0
            do{
                set = next
                next = mutableSetOf<String>()
                for (token in set){
                    next.add(token)
                    totalIndex[token]?.forEach{ it -> next.add(it)}
                }
                level++
            }while (set != next && level < maxLevels)
            return set
        }

        fun processAnnotatedTokens(annotatedTokens: List<MutableSet<String>>): List<MutableSet<String>> {
            return annotatedTokens.map {expand_token_set(it)}
        }

        val expanded = expand_token_set(setOf("for"))
        println("${setOf("for")} expands to ${expanded}")

        val higherLevelTokens = processAnnotatedTokens(base_phrase.split(' ').map{it -> mutableSetOf(it)})
        println("${base_phrase} maps to ${higherLevelTokens}")

        fun getTokenMappingToCanonicalToken(annotatedTokens:MutableSet<String>, handlerSpec:ClustSpec, nullToken:String = "<NULL>"): String? {
            return annotatedTokens.find { token -> handlerSpec.synonmys.containsKey(token)}
        }

        val speechWorldLine = WorldLine()

        val prevCanonicalTokenMap = mutableMapOf<String, String>()
        val updatedHandlerSet = mutableSetOf<String>()

        for ((i, tokenSet) in higherLevelTokens.withIndex()){
            for (rawToken in tokenSet){
                val handler = handlerIndex[rawToken]
                if (handler != null) {
                    // This is a higher order token name, now find a lower order token that
                    // can be mapped to a canonical token from handler
                    val mappingToken = getTokenMappingToCanonicalToken(tokenSet, handler)

                    if (mappingToken !=null){
                        // Found this token, now map it to its synonym and add to worldline
                        val canonicalToken = handler.synonmys[mappingToken]!!
                        speechWorldLine.setValue(rawToken, mappingToken to canonicalToken, i.toLong())
                        prevCanonicalTokenMap[rawToken] = canonicalToken
                    }
                    else {
                        if (prevCanonicalTokenMap[rawToken] != null) {
                            // Finished last argument segment
                            val lastTokenSegment = speechWorldLine.getAllValuesSinceBirth(rawToken, i - 1L).map { cog -> (cog as ValueCogject).name }
                            // Process this segment
                            println("Finished argument ${lastTokenSegment} for ${handler.name}")
                        }
                        prevCanonicalTokenMap.remove(rawToken)
                    }
                }
                else {
                    prevCanonicalTokenMap.remove(rawToken)
                    if (speechWorldLine.hasValue(rawToken, i.toLong())){
                        val lastTokenSegment = speechWorldLine.getAllValuesSinceBirth(rawToken, i - 1L).map { cog -> (cog as ValueCogject).name }
                        // Process this segment
                        println("Finished argument ${lastTokenSegment} for ${rawToken}")
                        speechWorldLine.expireKey(rawToken, i.toLong())
                    }
                }
            }
        }

        val lastTime = speechWorldLine.getLastTime();
        if (lastTime!=null){
            val handlerInputs = mutableMapOf<String, List<Pair<String, String>>>()
            for (handlerName in speechWorldLine.getState(lastTime).keys){
                handlerInputs[handlerName] = speechWorldLine.getAllValuesSinceBirth(handlerName, lastTime).map { cog -> (cog as ValueCogject).getValue() as Pair<String, String> }
            }
            println("Final output: ${handlerInputs}")
        }

//        fun getAllViableSpeechHandlerSpecs(annotatedSpeechList:List<MutableSet<String>>, totalClusters:List<ClustSpec>): Map<String, List<String>> {
//
//        }

    }


    @Test fun test_word_partitioning(){
        fun MutableMap<String, Int>.incrMap(key: String, defaultValue:Int = 0, weight: Int = 1) : MutableMap<String, Int> {
            if (containsKey(key)){
                put(key, weight + get(key)!!)
            }
            else {
                put(key, defaultValue + weight)
            }
            return this
        }


        val clusters = listOf<ClustSpec>(
                makeCluster("move-forward-centi", "move forward by 10 centimeters", mapOf("for" to "by", "go" to "move", "ten" to "10")),
                makeCluster("do-something-for-duration-centi", "by 10 centimeters", mapOf("for" to "by", "ten" to "10")),
                makeCluster("number-10", "10", mapOf("ten" to "10")),

                makeCluster("move-backward-centi", "move backward by 10 centimeters", mapOf("for" to "by", "go" to "move", "ten" to "10")),
                makeCluster("move-backward", "move backward", mapOf("for" to "by", "go" to "move")),
                makeCluster("move-forward", "move forward", mapOf("for" to "by", "go" to "move")),

                makeCluster("rotate-left-seconds", "rotate left for 2 seconds", mapOf("turn" to "rotate", "two" to "2")),
                makeCluster("rotate-left", "rotate left", mapOf("turn" to "rotate")),
                makeCluster("do-something-for-duration-seconds", "for 2 seconds", mapOf("two" to "2")),
                makeCluster("duration-seconds", "2 seconds", mapOf("two" to "2")),
                makeCluster("number-2", "2", mapOf("two" to "2")),

                makeCluster("rotate-right-seconds", "rotate right for 2 seconds", mapOf("turn" to "rotate", "two" to "2")),
                makeCluster("rotate-right", "rotate right", mapOf("turn" to "rotate")),

                makeCluster("rotate-right-degrees", "rotate right for 45 degrees", mapOf("for" to "by", "turn" to "rotate")),
                makeCluster("do-something-over-degrees-duration", "for 45 degrees", mapOf("for" to "by", "forty-five" to "45")),
                makeCluster("degrees-duration", "45 degrees", mapOf("forty-five" to "45")),
                makeCluster("number-45", "45", mapOf("forty-five" to "45"))
        )

        val includeAll = false
        val includeMax = true
        val includeMin = true

        var phrase = "move forward by 10 centimeters"

        var index = buildIndex(clusters)

        var matchMap = mutableMapOf<String, Int>()
        var prevSet:Set<String>? = null

        for (token in phrase.split(' ')) {
            val matchSet:Set<String>? = index[token]

            if (matchSet != null){
                var minKeys = mutableMapOf<String, Int>()
                var maxKeys = mutableMapOf<String, Int>()

                for (clusterName in matchSet){
                    matchMap.incrMap(clusterName)

                    if (minKeys.isEmpty() || (minKeys.iterator().next().value > matchMap[clusterName]!!)){
                        minKeys[clusterName] = matchMap[clusterName]!!
                    }
                    else if (minKeys.iterator().next().value == matchMap[clusterName]!!){
                        minKeys[clusterName] =  matchMap[clusterName]!!
                    }


                    if (maxKeys.isEmpty() || (maxKeys.iterator().next().value < matchMap[clusterName]!!)){
                        maxKeys[clusterName] = matchMap[clusterName]!!
                    }
                    else if (maxKeys.iterator().next().value == matchMap[clusterName]!!){
                        maxKeys[clusterName] =  matchMap[clusterName]!!
                    }
                }

                for (clusterName in matchSet) {
                    if (includeAll)
                        continue

                    if (includeMax && maxKeys.containsKey(clusterName)){
                        continue
                    }

                    if (includeMin && minKeys.containsKey(clusterName))
                        continue

                    matchMap.remove(clusterName)
                }

                prevSet?.forEach{priorClusterName ->
                    if (!matchSet.contains(priorClusterName)) {
                        matchMap.remove(priorClusterName)
                    }
                }

                prevSet = matchSet
            }
            else {
                prevSet = null
                matchMap = mutableMapOf<String, Int>()
            }

            println("matching ${token} -> ${matchMap.keys.sorted()}")
        }
    }





    @Test
    fun test_parsing_single_verb_speech_intent(){
        var speechWorld = WorldLine()
        val speechText = "move"
        val time = 0L

        addInitialSpeech(speechWorld, speechText, time)
        assertTrue("Speech data key should be present", speechWorld.hasValue("speech", 0))


        assertTrue("Speech processor should exist", speechWorld.hasValue(SPEECH_PROCESSING_COGJECT_NAME, time+1))

    }

    fun addInitialSpeech(world:WorldLine, speech:String, time:Long): Long {

        val parts = speech.split(' ')
        val earliestSpeechProcessingTime = time+parts.size
        for (t in time `until` earliestSpeechProcessingTime){
            world.setValue(makeCogject("speech", parts[t.toInt()])!!, t)
        }

        return earliestSpeechProcessingTime
    }

    fun processSpeech(world:WorldLine, spoken:String, speakTime: Long){

        // Allow null pointer exception as a test failure

    }

    @Test
    fun add_phrases_to_worldline(){
        val phrases = arrayOf("", "move", "go", "rotate", "move forward", "go forward", "rotate left by 45 degrees", "by 45 degrees", "go forward for 20 seconds", "move backward for 18 centimeters")
        var speechWorld = WorldLine()

        for (name: String in phrases){
            val earliest_speech_cogject_time:Long = speechWorld.say(name, 0)

            assertTrue("", earliest_speech_cogject_time == name.split(' ').size.toLong())
        }
    }

    @Test
    fun add_phrases_as_speech_cogjet(){
        val phrases = arrayOf("", "move", "go", "rotate", "move forward", "go forward", "rotate left by 45 degrees", "by 45 degrees", "go forward for 20 seconds", "move backward for 18 centimeters")
        var speechWorld = WorldLine()

        fun say(speechWorld:WorldLine, speech:String): Int {
            val tokens = speech.split(' ')
            for ((t, token) in tokens.withIndex()){
                speechWorld.setValue(makeCogject("speech", token)!!, t.toLong())
            }
            return tokens.size
        }

        for (name: String in phrases){
            val earliest_speech_cogject_time = say(speechWorld, name)

            assertTrue("", earliest_speech_cogject_time == name.split(' ').size)
        }
    }

    @Test
    fun add_phrases_as_speech(){
        var speechWorld = WorldLine()

        val phrases = arrayOf("", "move", "go", "rotate", "move forward", "go forward", "rotate left by 45 degrees", "by 45 degrees", "go forward for 20 seconds", "move backward for 18 centimeters")

        data class SpeechSpec (val word:String, val pos: Int, var claimants:MutableSet<String>)
        val phrase = phrases[4]
        val annotatedSpeechList = mutableListOf(phrase.split(' ').mapIndexed { i:Int, token:String -> SpeechSpec(token, i, mutableSetOf())})

        val tokenMatchClusterHandlerMap: MutableMap<String, ()->Cogject> = mutableMapOf(
                "move forward" to { -> makeCogject("moving forward", true)!!},
                "move backward" to { -> makeCogject("moving backward", true)!!})

        class HandlerSpec (val name: String, var canonicalNameMap:MutableMap<String, String>, val tokens:Array<String>) {
            init {
                tokens.forEach {token:String ->
                    canonicalNameMap[token] = token
                }
            }
        }

        val invertedIndex = mutableMapOf<String, MutableSet<String>>()
    }


    @Test fun testBuildRobustMatchSequence(){

        var phrase = "move forward for 10 seconds"

        val tokens = phrase.split(' ')

        fun MutableMap<String, Int>.incrMap(key: String, defaultValue:Int = 0, weight: Int = 1) : MutableMap<String, Int> {
            if (containsKey(key)){
                put(key, weight + get(key)!!)
            }
            else {
                put(key, defaultValue + weight)
            }
            return this
        }

        fun Map<String, Int>.incrMap(key:String, defaultValue:Int = 0, weight: Int = 1): Map<String, Int> {
            if (!containsKey(key)){
                return mutableMapOf(*entries.map { (k, i) -> if (k == key) k to (i + weight) else k to i}.toTypedArray()).apply { put(key, defaultValue+weight) }.toMap()
            }
            else
                return mapOf(*entries.map { (k, i) -> if (k == key) k to (i + weight) else k to i}.toTypedArray())
        }

        val expectedSets = listOf<Map<String, Int>>(mapOf("move" to 1), mapOf("move" to 1, "forward" to 1), mapOf("forward" to 1, "for" to 1), mapOf("for" to 1, "10" to 1), mapOf("10" to 1, "seconds" to 1), mapOf("seconds" to 1))

        val windowSize = 1


        val expected = listOf(
                "move" to listOf(mapOf("move" to 1)),
                "move forward" to listOf(mapOf("move" to 1), mapOf("move" to 1, "forward" to 1)),
                "move forward for 10 seconds" to listOf<Map<String, Int>>(mapOf("move" to 1), mapOf("move" to 1, "forward" to 1), mapOf("forward" to 1, "for" to 1), mapOf("for" to 1, "10" to 1), mapOf("10" to 1, "seconds" to 1))
                )

        fun buildMatchSetList(words:List<String>): List<Map<String, Int>> {
            return words.mapIndexed {i, _ ->
                val mapForIndex = mutableMapOf<String, Int>()
                for (j in max(0, i - windowSize)..i) {
                    mapForIndex.incrMap(words[j])
                }
                mapForIndex.toMap()
            }
        }

        val matchSetList = buildMatchSetList(tokens)

        assertTrue("matching ${expected[0].second}", expected[0].second == buildMatchSetList(expected[0].first.split(' ')))

        assertTrue("matching ${expected[1].second}", expected[1].second == buildMatchSetList(expected[1].first.split(' ')))

        for ((phrase, expectedMap) in expected){
            val tokens = phrase.split(' ')
            assertTrue("matching $phrase}", expectedMap == buildMatchSetList(tokens))
        }


    }

    fun buildTokenOrderMapsList(words:List<String>, windowSize:Int = words.size): List<Map<String, Int>> {
        fun MutableMap<String, Int>.incrMap(key: String, defaultValue:Int = 0, weight: Int = 1) : MutableMap<String, Int> {
            if (containsKey(key)){
                put(key, weight + get(key)!!)
            }
            else {
                put(key, defaultValue + weight)
            }
            return this
        }

        return words.mapIndexed {i, _ ->
            val mapForIndex = mutableMapOf<String, Int>(*words.map{word -> word to 0}.toTypedArray())
            for (j in max(0, i - windowSize)..i) {
                mapForIndex.incrMap(words[j])
            }
            mapForIndex.toMap()
        }
    }

    fun getMatchScore(matchSpec:List<Map<String, Int>>, speech:List<String>): Float {
        fun MutableMap<String, Int>.incrMap(key: String, defaultValue:Int = 0, weight: Int = 1) : MutableMap<String, Int> {
            if (containsKey(key)){
                put(key, weight + get(key)!!)
            }
            else {
                put(key, defaultValue + weight)
            }
            return this
        }

        var maxScore = max(matchSpec.size, speech.size)

        var compMap = mutableMapOf<String, Int>()
        var score = 0.0F
        for ((i, word) in speech.withIndex()){
            compMap.incrMap(word)
            val specMap = matchSpec[i]
            if (specMap.containsKey(word)){
                val wordWeight = 1.0F
                score+=wordWeight/(1.0F + abs(compMap[word]!! - specMap[word]!!))
            }
        }
        return score/maxScore
    }

    @Test fun testScoringAPhrase(){
        val canonicalPhrase = "move forward for 20 seconds"

        val matchSpec = buildTokenOrderMapsList(canonicalPhrase.split(' '), 2)

        val testSpeech = listOf("", canonicalPhrase, "go", "move", "move forward", "move forward for", "move forward for 20 seconds", "seconds", "seconds 20 for", "seconds 20 for move")

        var score = getMatchScore(matchSpec, testSpeech[0].split(' '))

        assertTrue("Expected zero score for empty score", score == 0.0F)

        for (phrase in testSpeech){
            score = getMatchScore(matchSpec, phrase.split(' '))
            println("Score of \"${phrase}\" against \"${canonicalPhrase}\" is ${score}")
        }
    }

    @Test
    fun test_match_vialble_clusters()  {

        val phrases = arrayOf("", "move", "go", "rotate", "move forward", "go forward", "rotate left by 45 degrees", "by 45 degrees", "go forward for 20 seconds", "move backward for 18 centimeters")

        data class MatchStats(val matchOrderSets:List<Map<String, Int>>, var orderScore: Float?, var setScore:Float?, val numInputTokens:Int)
        data class ClustSpec (val phrase:String, val name:String, var synonmys:MutableMap<String, String> = mutableMapOf(), var stats: MatchStats?=null)
        data class matchTokenClusterCache (var index:MutableMap<String, MutableSet<String>>, val matchStats:MutableMap<String, Double>, val handlerMap:Map<String, ClustSpec>)

        fun addHandler(handler: ClustSpec, invertedIndex:MutableMap<String, MutableSet<String>>){
            handler.synonmys.forEach{(spokenWord:String, _:String)->
                var prior = invertedIndex?.get(spokenWord)?: mutableSetOf()
                prior.add(handler.name)
                invertedIndex[spokenWord] = prior
            }
        }

        fun buildIndex(handlers:List<ClustSpec>): MutableMap<String, MutableSet<String>> {
            val invertedIndex = mutableMapOf<String, MutableSet<String>>()

            for (spec:ClustSpec in handlers){
                addHandler(spec, invertedIndex)
            }
            return invertedIndex
        }

        fun filterViableClusters(tokenClusters:List<ClustSpec>, phrase: String): Set<String> {
            val out = mutableSetOf<String>()

            val words = phrase.split(' ')
            val index = buildIndex(tokenClusters)

            for (word in words){
                val candidates = index[word]
                if (candidates != null){
                    candidates.forEach {clusterName: String -> out.add(clusterName)}
                }
            }
            return out.toSet()
        }

        val tokenClusters = listOf(
                ClustSpec("move forward by 30 centimeters", "forward-30-centi", mutableMapOf(*("move forward by 30 centimeters".split(' ').map {x -> x to x}.toTypedArray()))),
                ClustSpec( "move backward by 20 seconds" , "back-20-secs", mutableMapOf(*("move backward by 20 seconds".split(' ').map {x -> x to x}.toTypedArray())))
        )



        var phrase = ""
        var viableClusters = filterViableClusters(tokenClusters, phrase)

        assertTrue("Should be no viable cluster for \"${phrase}\"", viableClusters.isEmpty())

        phrase = "move"

        viableClusters = filterViableClusters(tokenClusters, phrase)

        assertTrue("Expected 2 clusters for \"${phrase}\"", viableClusters.size == 2)
    }


    @Test
    fun test_match_token_cluster_explore()  {

        val phrases = arrayOf("", "move", "go", "rotate", "move forward", "go forward", "rotate left by 45 degrees", "by 45 degrees", "go forward for 20 seconds", "move backward for 18 centimeters")

        val tokenClusters = listOf(
                ClustSpec("move forward by 30 centimeters", "forward-30-centi", mutableMapOf(*("move forward by 30 centimeters".split(' ').map {x -> x to x}.toTypedArray()))),
                ClustSpec( "move backward by 20 seconds" , "back-20-secs", mutableMapOf(*("move backward by 20 seconds".split(' ').map {x -> x to x}.toTypedArray())))
        )

        val match = matchTokenCluster(tokenClusters, phrases[1])
        assertTrue("match should be empty", match.isNotEmpty())

        var phrase = "forward"
        assertTrue("should match \"${tokenClusters[0].name}\" with \"${phrase}\"", matchTokenCluster(tokenClusters, phrase) == setOf(tokenClusters[0].name))

        phrase = "backward"
        assertTrue("should match \"${tokenClusters[1].name}\" with \"${phrase}\"", matchTokenCluster(tokenClusters, phrase) == setOf(tokenClusters[1].name))
    }


    @Test fun testSpeechContexts(){
        val aiWorldLine = WorldLine()

        aiWorldLine.addValueCogject("height", 5.5F, 0L)
        aiWorldLine.addValueCogject("weight", 100, 0L)
        aiWorldLine.addValueCogject("age", 10, 0L)
        aiWorldLine.addValueCogject("name", "Andrew", 0L)

        val context = SpeechContext(
                nluWorldLine = aiWorldLine,
                speech = "what is height of Andrew",
                temporalOffset = 10L,
                handlers = mutableListOf<IntentHandler>(
                        makeSpeechIntent(
                                intentTokenName = "[query-property]",
                                phrase = "what is the <attribute> of <user-name>",
                                requiredCogjects = setOf("[attribute]", "[user-name]"),
                                outputCogjects = setOf("answer"),
                                handler = {tokens:List<String>, outputWorldLine:WorldLine, time:Long ->
                                    val nameTime:Pair<String, Long>? = outputWorldLine.getCogjectValue<Pair<String, Long>>("[user-name]", time)
                                    if (nameTime == null) {
                                        HANDLER_STATE.FAILURE
                                    }
                                    else {
                                        val attributeName = outputWorldLine.getCogjectValue<String>("[attribute]", time)
                                        if (attributeName == null){
                                            HANDLER_STATE.FAILURE
                                        }
                                        else {
                                            val requiredTime = nameTime.second
                                            val objectName = nameTime.first
                                            val attributeValue =outputWorldLine.getCogjectValue(attributeName, requiredTime) as Any?
                                            if (attributeValue == null){
                                                println("The ${attributeName} of ${objectName} is undefined")
                                            }
                                            else
                                                println("The ${attributeName} of ${objectName} is ${attributeValue}")

                                            HANDLER_STATE.SUCCESS
                                        }

                                    }

                                }
                                ),
                        makeSpeechIntent(
                                intentTokenName = "[attribute]",
                                phrase = "attribute",
                                synonymMap = mutableMapOf("height" to "attribute", "weight" to "attribute","age" to "attribute"),
                                basePredicates = setOf("<attribute>"),
                                maxGapSize = 0,
                                handler = {tokens:List<String>, outputWorldLine:WorldLine, time:Long ->
                                    val attributeName = tokens[0]
                                    if (attributeName != "attribute"){
                                        outputWorldLine.addValueCogject(intentName, attributeName, time)
                                        HANDLER_STATE.SUCCESS
                                    }
                                    else
                                        HANDLER_STATE.FAILURE}
                                ),
                        makeSpeechIntent(
                                intentTokenName = "[user-name]",
                                phrase = "name",
                                synonymMap = mutableMapOf("Andrew" to "name", "Aprill" to "name","Eddie" to "name"),
                                basePredicates = setOf("<user-name>"),
                                handler = {tokens:List<String>, outputWorldLine:WorldLine, time:Long->
                                    if (outputWorldLine.hasValue("name", time)){
                                        val name = outputWorldLine.getCogjectValue<String>("name", time)
                                        val createTime = outputWorldLine.getLastEntry("name", time)!!.updateTime
                                        outputWorldLine.addValueCogject(intentName, Pair(name, createTime), time)
                                        HANDLER_STATE.SUCCESS
                                    }
                                    else
                                        HANDLER_STATE.FAILURE
                                })

                )
        )

        var result = context.processSpeech()
        println("Processing speech ${context.speech} resulting in ${result} world state is: ${aiWorldLine.getState()}")
        result = context.processSpeech("what is the weight of Aprill")
        println("Result: ${result}.  speech mind is: ${context.speechProcessingWorld!!.getState()}")
    }


    @Test fun testSpeechErrors(){
        val aiWorldLine = WorldLine()

        aiWorldLine.addValueCogject("height", 5.5F, 0L)
        aiWorldLine.addValueCogject("weight", 100, 0L)
        aiWorldLine.addValueCogject("age", 10, 0L)
        aiWorldLine.addValueCogject("name", "Andrew", 0L)

        val context = SpeechContext(
                nluWorldLine = aiWorldLine,
                speech = "what is the weight of Aprill",
                temporalOffset = 10L,
                handlers = mutableListOf<IntentHandler>(
                        makeSpeechIntent(
                                intentTokenName = "[query-property]",
                                phrase = "what is the <attribute> of <user-name>",
                                requiredCogjects = setOf("[attribute]", "[user-name]"),
                                outputCogjects = setOf("answer"),
                                handler = {tokens:List<String>, outputWorldLine:WorldLine, time:Long ->
                                    val nameTime:Pair<String, Long>? = outputWorldLine.getCogjectValue<Pair<String, Long>>("[user-name]", time)
                                    if (nameTime == null) {
                                        HANDLER_STATE.FAILURE
                                    }
                                    else {
                                        val attributeName = outputWorldLine.getCogjectValue<String>("[attribute]", time)
                                        if (attributeName == null){
                                            HANDLER_STATE.FAILURE
                                        }
                                        else {
                                            val requiredTime = nameTime.second
                                            val objectName = nameTime.first
                                            val attributeValue =outputWorldLine.getCogjectValue(attributeName, requiredTime) as Any?
                                            if (attributeValue == null){
                                                println("The ${attributeName} of ${objectName} is undefined")
                                            }
                                            else
                                                println("The ${attributeName} of ${objectName} is ${attributeValue}")

                                            HANDLER_STATE.SUCCESS
                                        }

                                    }

                                }
                        ),
                        makeSpeechIntent(
                                intentTokenName = "[attribute]",
                                phrase = "attribute",
                                synonymMap = mutableMapOf("height" to "attribute", "weight" to "attribute","age" to "attribute"),
                                basePredicates = setOf("<attribute>"),
                                maxGapSize = 0,
                                handler = {tokens:List<String>, outputWorldLine:WorldLine, time:Long ->
                                    val attributeName = tokens[0]
                                    if (attributeName != "attribute"){
                                        outputWorldLine.addValueCogject(intentName, attributeName, time)
                                        HANDLER_STATE.SUCCESS
                                    }
                                    else
                                        HANDLER_STATE.FAILURE}
                        ),
                        makeSpeechIntent(
                                intentTokenName = "[user-name]",
                                phrase = "name",
                                synonymMap = mutableMapOf("Andrew" to "name", "Aprill" to "name","Eddie" to "name"),
                                basePredicates = setOf("<user-name>"),
                                handler = {tokens:List<String>, outputWorldLine:WorldLine, time:Long->
                                    val expectedName = tokens.find {t -> t in synonoyms}
                                    if (outputWorldLine.hasValue("name", time) && expectedName != null ){
                                        val name = outputWorldLine.getCogjectValue<String>("name", time)
                                        if (name == expectedName){
                                            val createTime = outputWorldLine.getLastEntry("name", time)!!.updateTime
                                            outputWorldLine.addValueCogject(intentName, Pair(name, createTime), time)
                                            HANDLER_STATE.SUCCESS
                                        }
                                        else {
                                            println("I don't know anything about ${expectedName}")
                                            HANDLER_STATE.FAILURE
                                        }
                                    }
                                    else {
                                        if (expectedName == null) {
                                            println("I don't know who ${tokens[0]} is")
                                        }
                                        HANDLER_STATE.FAILURE
                                    }
                                })

                )
        )

        var result = context.processSpeech()
        //println("Processing speech ${context.speech} resulting in ${result} world state is: ${aiWorldLine.getState()}")

        println("Result: ${result}.  speech mind is: ${context.speechProcessingWorld!!.getState()}")


        result = context.processSpeech("What is the weight of johnny")

        println("Result: ${result}.  speech mind is: ${context.speechProcessingWorld!!.getState()}")

        result = context.processSpeech("What is the weight of Andrew")

        println("Result: ${result}.  speech mind is: ${context.speechProcessingWorld!!.getState()}")
    }

    @Test fun testSinglePartialIntervalNarrativeTesting(){
        val worldline = WorldLine()
        val created = worldline.addValueCogject("x", "a", 1)
        val output = worldline.getAllNarratives(1, 10, 0)
        val expected = WorldLine.CogjectEntry(1, created)
        assertTrue("Expected partial narrative cover start of cogject, x", output.size == 1 && output[0].contains(expected))
    }

    @Test fun testSingleIntervalNarativeBuilding(){
        val worldline = WorldLine()

        fun createInterval(name:String, value:Any, start:Long, end:Long): Cogject{
            val cog = worldline.addValueCogject(name, value, start)
            worldline.expireKey(name, end)
            return cog
        }

        val cog = createInterval("x", 1, 1, 10)

        var output = worldline.getAllNarratives(1, 10, 0)

        val expected = WorldLine.CogjectEntry(1L, cog)

        assertTrue("Expected a single narrative covering entire interval", output.size == 1 && output[0].contains(expected))

        output = worldline.getAllNarratives(-10, 50, 0)
        assertTrue("Expected empty narratives for misaligned interval", output.isEmpty())

        output = worldline.getAllNarratives(3, 10, 3)
        assertTrue("Expected fuzzy narrative to exist for start", output.size == 1 && output[0].contains(expected))

        output = worldline.getAllNarratives(1, 13, 3)
        assertTrue("Expected fuzzy narrative to exist for end", output.size == 1 && output[0].contains(expected))

        output = worldline.getAllNarratives(-2, 13, 3)
        assertTrue("Expected fuzzy narrative to exist for both ends", output.size == 1 && output[0].contains(expected))

        //createInterval("y", 1, 10, 20)
        //createInterval("z", 1, 20, 30)

    }


    @Test fun testMultipleIntervalNarratives(){

        val world = WorldLine()

        val stack = Thread.currentThread().stackTrace

        fun createInterval(name:String, value:Any, start:Long, end:Long): WorldLine.CogjectEntry {
            val cog = world.addValueCogject(name, value, start)
            world.expireKey(name, end)
            return WorldLine.CogjectEntry(start, cog)
        }

        val first = createInterval("x", "a", 10, 30)
        val second = createInterval("y", "b", 10, 30)

        var out = world.getAllNarratives(10, 30, 0)

        assertTrue("Expected two independent narratives for x and y", out.size == 2)

        val third = createInterval("z", "c", 30, 60)
        out = world.getAllNarratives(10, 60, 0)

        assertTrue("Expected double narrative covering sequence of [x,z]", out.size == 2)

        println("Narrative: ${out}")
    }

}