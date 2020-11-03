package com.evolved.automata

import com.evolved.automata.nn.util.UnsignedNumVectorType
import kotlin.math.abs
import kotlin.math.max


fun log2(v: Double) = Math.log10(v)/Math.log10(2.0)

fun log2(v: Int) = Math.log10(v.toDouble())/Math.log10(2.0)

class FiniteSet(val maxSize: Int) {
    private var things = Array<Any?>(maxSize){null}
    private var valueMap =  mutableMapOf<Any, Int >()
    private val vectorType: UnsignedNumVectorType

    init {
        val width = 1 + Math.floor(log2(maxSize))

        vectorType = UnsignedNumVectorType(width.toInt())
    }


    fun add(value: Any): Int? {
        val prior = valueMap[value]
        if (prior != null){
            valueMap[value] = prior
            things[prior] = value
            return prior
        }
        val loc = getAnotherIndex()
        if (loc != null) {
            valueMap[value] = loc
            things[loc] = value
            return loc
        }
        return null
    }

    val size get() = valueMap.size

    fun getAnotherIndex(): Int? {
        if (valueMap.size < things.size){
            return valueMap.size
        }
        else {
            val indexes = things.withIndex().filter { iv: IndexedValue<Any?> -> iv.value == null }.map {iv -> iv.index}

            return if (indexes.size > 0) indexes[0] else null
        }
    }



    fun makeValueCogject(name: String, value: Any, action: (Cogject.(world:WorldLine, time: Long)->FloatArray)? = null) : ValueCogject? {
        val index = add(value)

        if (index != null) {
            val item = things[index]!!
            return object: ValueCogject(type = vectorType, name = name, value = index){
                override fun getValue(): Any {
                    return item
                }

                override fun toString(): String {
                    return "(\"$name\", ${getValue()})"
                }

                override fun copy(): Cogject {
                    return makeValueCogject(name, value, action) as Cogject
                }

                override fun processInternal(world: WorldLine, processTime: Long): Unit {
                    if (action == null)
                        super.processInternal(world, processTime)
                    else
                        action(world, processTime)
                }
            }
        }
        else {
            return null
        }
    }
}


// ///////////..............////////////////.............///////////
//              Speech Functions
val globalSet = FiniteSet(10000)


data class IntentHandler (val intentName:String, val phraseTokens:List<String>, val synonoyms:MutableMap<String, String>, val maxMatchGapSize: Int? = null, val parsePredicates:MutableSet<String> = mutableSetOf(), val requiredCogjectNames:Set<String>, val outputCogjectNames:Set<String>, val predicates:MutableSet<String> = mutableSetOf(), var multiWordTokenIndex:MutableMap<String, MutableSet<String>>? = null , val handler:IntentHandler.(tokens:List<String>, stateWorldline: WorldLine, time:Long) -> HANDLER_STATE = { tokens, stateWorldline, time  -> stateWorldline.setValue(makeCogject(intentName, intentName), time);   HANDLER_STATE.SUCCESS }, val defaultMatchFraction:Float = 0.1F)
enum class HANDLER_STATE { BUILDING, PROCESSING, WAITING, FAILURE, SUCCESS}
enum class TOKENIZATION_METHOD {GREEDY, RANDOM, BASIC}

fun MutableMap<String, MutableSet<String>>.incrListSet(key:String, item:String):MutableMap<String, MutableSet<String>> {
    val prior = get(key)
    if (prior == null){
        put(key, mutableSetOf(item))
    }
    else {
        prior.add(item)
    }
    return this
}

fun MutableMap<String, Int>.incrMap(key: String, defaultValue:Int = 0, weight: Int = 1) : MutableMap<String, Int> {
    if (containsKey(key)){
        put(key, weight + get(key)!!)
    }
    else {
        put(key, defaultValue + weight)
    }
    return this
}

fun makeCogject(name:String, value:Any): Cogject
{
    return globalSet.makeValueCogject(name, value)!!
}


fun <T> List<T>.matchingIndexInRange(start:Int, width:Int, predictate:(T) -> Boolean): Int? {

    var i  = start
    while (i < Math.min(start+width, size)) {
        if (predictate(get(i))){
            return i
        }
        i++
    }

    return null
}

fun IntentHandler.tokenize(speechInput:String, tokenizationMethod:TOKENIZATION_METHOD = TOKENIZATION_METHOD.GREEDY) : List<String> {
    if (this.multiWordTokenIndex == null) {
        this.multiWordTokenIndex = mutableMapOf()

        fun addSynonymToken(token:String){
            val parts = token.split(' ')
            multiWordTokenIndex?.incrListSet(parts[0], token)
        }

        synonoyms.keys.forEach {synonym -> addSynonymToken(synonym)}
    }

    when (tokenizationMethod) {
        TOKENIZATION_METHOD.BASIC -> return speechInput.split(' ')
        TOKENIZATION_METHOD.GREEDY -> {
            val tokens = mutableListOf<String>()
            var remaining = speechInput.trim()
            while (remaining.length >0){
                val space = remaining.indexOf(" ")
                val prefixIndex:String = if (space != -1) remaining.substring(0, space) else remaining
                val possibleTokens:MutableSet<String>? = multiWordTokenIndex!![prefixIndex]
                if (possibleTokens == null){
                    tokens.add(prefixIndex)
                    remaining =remaining.substring(prefixIndex.length).trim()
                }
                else {
                    var longest: String? = null
                    for (suffix in possibleTokens){
                        if (remaining.startsWith(suffix)){
                            if (longest == null || suffix.length > longest.length){
                                longest = suffix
                            }
                        }
                    }

                    if (longest != null){
                        tokens.add(longest)
                        remaining = remaining.substring(longest.length).trim()
                    }
                    else {
                        tokens.add(prefixIndex)
                        remaining = remaining.substring(prefixIndex.length).trim()
                    }
                }

            }
            return tokens
        }
        TOKENIZATION_METHOD.RANDOM -> {
            val tokens = mutableListOf<String>()
            var remaining = speechInput
            while (remaining.trim().length >0){
                val space = remaining.indexOf(" ")
                val prefixIndex:String = if (space != -1) remaining.substring(0, space) else remaining
                val possibleTokens:MutableSet<String>? = multiWordTokenIndex!![prefixIndex]
                if (possibleTokens==null){
                    remaining = remaining.substring(prefixIndex.length).trim()
                }
                else {
                    val selected = possibleTokens.filter { t -> remaining.startsWith(t)}.random()
                    tokens.add(selected)
                    remaining = remaining.substring(selected.length).trim()
                }
            }

            return tokens
        }
    }
}

fun isHigherToken(tokenName:String):Boolean {
    return tokenName.startsWith('[') && tokenName.endsWith(']')
}

fun isPredicateToken(tokenName:String):Boolean {
    return tokenName.startsWith('<') && tokenName.endsWith('>')
}

fun makeSpeechIntent(
        intentTokenName:String,
        phrase:String,
        synonymMap:MutableMap<String, String>? = null,
        maxGapSize:Int? = null,
        parseRequirements:MutableSet<String> = mutableSetOf(),
        requiredCogjects:Set<String>? = null,
        outputCogjects:Set<String>? = null,
        basePredicates:Set<String>? = null,
        handler:IntentHandler.(tokens:List<String>, outputWorldLine:WorldLine, time:Long) -> HANDLER_STATE = { tokens, stateWorldline:WorldLine, time  ->   stateWorldline.setValue(makeCogject(intentTokenName, intentTokenName), time); HANDLER_STATE.SUCCESS },
        defaultMatchThreshold:Float? = null): IntentHandler {
    val tokens = phrase.split(' ')
    val syn = (mutableMapOf(*tokens.map {it -> it to it}.toTypedArray())).apply{putAll( synonymMap?:mutableMapOf<String, String>())}


    return IntentHandler(intentName = intentTokenName,
            phraseTokens = tokens,
            synonoyms = syn,
            maxMatchGapSize = maxGapSize,
            parsePredicates = parseRequirements,
            requiredCogjectNames = mutableSetOf(*tokens.filter { t -> isHigherToken(t)}.toTypedArray()).apply {addAll(requiredCogjects?: setOf()) },
            outputCogjectNames = mutableSetOf(intentTokenName).apply {addAll(outputCogjects?: setOf())},
            predicates = mutableSetOf<String>().apply{addAll(basePredicates?: setOf())},
            handler = handler,
            defaultMatchFraction = defaultMatchThreshold?:1.0F/tokens.size
    )
}



open class SpeechContext(val speech:String?, var temporalOffset:Long = 0, var phraseTokens:List<String>? = null, var handlers:MutableList<IntentHandler>, var searchWidth:Int = 2, val nluWorldLine:WorldLine, var speechProcessingWorld:WorldLine? = null, var speechArgumentWorld:WorldLine? = null) {

    private val multiWordTokenIndex: MutableMap<String, MutableSet<String>> = mutableMapOf()

    var handlerIndex: MutableMap<String, IntentHandler>? = null

    val handlerMap = mutableMapOf<String, IntentHandler>(*handlers.map {it -> it.intentName to it}.toTypedArray())

    var tokenInvertedIndex:MutableMap<String, MutableSet<String>>? = null

    var annotatedTokens:List<MutableSet<String>>? = null

    init {

        if (tokenInvertedIndex == null){
            tokenInvertedIndex = buildIndex()
        }

        for (handler in handlers){
            for (syn in handler.synonoyms.keys){
                addSynonymToken(syn)
            }
        }

        if (handlerIndex == null) {
            updateIndex()
        }

        if (speechProcessingWorld == null || speechArgumentWorld == null) {
            val finiteSet = FiniteSet(200)
            if (speechArgumentWorld == null)
                speechArgumentWorld = WorldLine(finiteSet)
            if (speechProcessingWorld == null)
                speechProcessingWorld = WorldLine(finiteSet)
        }

        if (phraseTokens == null){
            if (speech != null){
                phraseTokens = tokenize(speech)
            }

            if (phraseTokens != null){
                if (annotatedTokens == null){
                    annotatedTokens = phraseTokens?.map { spokenToken -> expandTokenSet(setOf(spokenToken))}
                }
            }
        }

        if (annotatedTokens == null){
            annotatedTokens = phraseTokens?.map { spokenToken -> expandTokenSet(setOf(spokenToken))}
        }
    }

    fun rebuildSpeechModelIndexes(){
        tokenInvertedIndex = buildIndex()

        for (handler in handlers){
            for (syn in handler.synonoyms.keys){
                addSynonymToken(syn)
            }
        }

        updateIndex()
    }

    fun resetSpeechResultState(){
        val finiteSet = FiniteSet(200)
        speechArgumentWorld = WorldLine(finiteSet)
        speechProcessingWorld = WorldLine(finiteSet)
    }

    fun updateIndex(){
        handlerIndex = mutableMapOf(*handlers.map { handler -> handler.intentName to handler}.toTypedArray())
    }

    fun addSynonymToken(token:String){
        val parts = token.split(' ')
        multiWordTokenIndex.incrListSet(parts[0], token)
    }

    fun tokenize(phrase:String, tokenizationMethod:TOKENIZATION_METHOD = TOKENIZATION_METHOD.GREEDY): List<String>{
        when (tokenizationMethod) {
            TOKENIZATION_METHOD.BASIC -> return phrase.split(' ')
            TOKENIZATION_METHOD.GREEDY -> {
                val tokens = mutableListOf<String>()
                var remaining = phrase.trim()
                while (remaining.length >0){
                    val space = remaining.indexOf(" ")
                    val prefixIndex:String = if (space != -1) remaining.substring(0, space) else remaining
                    val possibleTokens:MutableSet<String>? = multiWordTokenIndex[prefixIndex]
                    if (possibleTokens == null){
                        tokens.add(prefixIndex)
                        remaining =remaining.substring(prefixIndex.length).trim()
                    }
                    else {
                        var longest: String? = null
                        for (suffix in possibleTokens){
                            if (remaining.startsWith(suffix)){
                                if (longest == null || suffix.length > longest.length){
                                    longest = suffix
                                }
                            }
                        }

                        if (longest != null){
                            tokens.add(longest)
                            remaining = remaining.substring(longest.length).trim()
                        }
                        else {
                            tokens.add(prefixIndex)
                            remaining = remaining.substring(prefixIndex.length).trim()
                        }
                    }

                }
                return tokens
            }
            TOKENIZATION_METHOD.RANDOM -> {
                val tokens = mutableListOf<String>()
                var remaining = phrase
                while (remaining.trim().length >0){
                    val space = remaining.indexOf(" ")
                    val prefixIndex:String = if (space != -1) remaining.substring(0, space) else remaining
                    val possibleTokens:MutableSet<String>? = multiWordTokenIndex[prefixIndex]
                    if (possibleTokens==null){
                        remaining = remaining.substring(prefixIndex.length).trim()
                    }
                    else {
                        val selected = possibleTokens.filter { t -> remaining.startsWith(t)}.random()
                        tokens.add(selected)
                        remaining = remaining.substring(selected.length).trim()
                    }
                }

                return tokens
            }
        }
    }

    fun buildIndex(): MutableMap<String, MutableSet<String>> {
        fun addHandler(handler: IntentHandler, invertedIndex:MutableMap<String, MutableSet<String>>){
            handler.synonoyms.forEach{(spokenWord:String, _:String)->
                var prior = invertedIndex?.get(spokenWord)?: mutableSetOf()
                prior.add(handler.intentName)
                invertedIndex[spokenWord] = prior
            }
        }

        val invertedIndex = mutableMapOf<String, MutableSet<String>>()

        for (spec:IntentHandler in handlers){
            addHandler(spec, invertedIndex)
        }
        return invertedIndex
    }

    fun getTokenMappingToCanonicalToken(annotatedTokens:MutableSet<String>, handlerOfCanonicalToken:IntentHandler): String? {
        return annotatedTokens.find { token -> handlerOfCanonicalToken.synonoyms.containsKey(token)}
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

    fun getMatchScore(speech:List<String>, matchSpec:List<Map<String, Int>> = buildTokenOrderMapsList(speech, 3)): Float {
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

    fun getScoreThreshold(tokens:List<String>): Float {
        // Technical note #2: probably need to make this Intent specific but this is just a first
        // pass optimization that makes processing faster
        return (1/tokens.size).toFloat()
    }

    fun getCurrentInputTokens(higherToken:String, time:Number):MutableList<String> {
        var tokenInput = mutableListOf<String>()
        for (tokenCogject in speechArgumentWorld!!.getAllValuesSinceBirth(higherToken, time)){
            val token = (tokenCogject as ValueCogject).getValue() as String
            // Consecutive higher order or predicate token names are squashed
            if (!(isPredicateToken(token) || isHigherToken(token)) || tokenInput.isEmpty() || (tokenInput.size > 0 && tokenInput[tokenInput.lastIndex] != token)){
                tokenInput.add(token)
            }
        }
        return tokenInput
    }

    fun expandTokenSet(tokenSet: Set<String>, maxLevels:Int = 100): MutableSet<String> {
        var set = mutableSetOf<String>().apply {addAll(tokenSet)}
        var next:MutableSet<String> = set
        var level = 0
        do {
            set = next
            next = mutableSetOf<String>()
            for (token in set){
                next.add(token)
                tokenInvertedIndex!![token]?.forEach{
                    higherHandler -> next.add(higherHandler)
                    handlerMap[higherHandler]?.predicates?.forEach {predToken -> next.add(predToken)}
                }

            }
            level++
        } while (set != next && level < maxLevels)
        return set
    }

    fun getCurrentScore(higherToken:String, tokenInput:List<String>): Float {
        val handler = handlerMap[higherToken]!!
        val inputInCanonicalForm = tokenInput.map {it -> handlerMap[higherToken]!!.synonoyms[it]?:it}

        val canonicalInput = handler.phraseTokens
        // Technical note #1: this assumes a similar tolerance for word order mismatches between the speaker
        // and the IntentHandler
        if (inputInCanonicalForm.size <= canonicalInput.size){
            // this is interpreting the speech from the perspective of the IntentHandler
            // The IntentHandler expects a certain fuzzy sequence of tokens matches the spoken input
            // against this
            return getMatchScore(inputInCanonicalForm, buildTokenOrderMapsList(canonicalInput, handler.maxMatchGapSize?:searchWidth))
        }
        else {
            // This is interpreting the canonical phrase from the perspective of the speaker's spoken
            // input.  Techincally, the windowSize should be define based on the speaker's preference
            // instead of the IntentHandler's [maxMatchGapSize] but we are assuming similar tolerance
            // for mismatches as explained above
            return getMatchScore(canonicalInput, buildTokenOrderMapsList(inputInCanonicalForm, handler.maxMatchGapSize?:searchWidth))
        }

    }

    fun processSpeech(newSpeech:String? = null, offset:Long? = temporalOffset): Cogject? {
        if (newSpeech != null){
            phraseTokens = tokenize(newSpeech)
            annotatedTokens = phraseTokens?.map { spokenToken -> expandTokenSet(setOf(spokenToken))}

            resetSpeechResultState()
        }

        if (offset != null)
            temporalOffset = offset

        fun getScoreThreshold(tokens:List<String>, scoreThreshold:Float? = null): Float {
            // Technical note #2: probably need to make this Intent specific but this is just a first
            // pass optimization that makes processing faster
            if (scoreThreshold!=null)
                return scoreThreshold
            return (1/tokens.size).toFloat()
        }

        fun <T> Cogject.getValue(): T {
            return (this as ValueCogject).getValue() as T
        }

        fun processHigherTokenBoundary(higherToken:String, time: Number): List<String> {
            var deferredSpeechInput:List<String>? = null
            if (speechProcessingWorld!!.getCogjectValue<HANDLER_STATE>(higherToken, time) == HANDLER_STATE.WAITING){
                deferredSpeechInput = speechProcessingWorld!!.getCogjectValue("${higherToken}.speech-tokens", time)
            }

            var tokenInput = mutableListOf<String>()
            if (deferredSpeechInput != null){
                tokenInput.addAll(deferredSpeechInput)
            }
            else {
                tokenInput = getCurrentInputTokens(higherToken, time)
            }

            // score the tokenInput

            val canonicalInput = tokenInput.map {it -> handlerMap[higherToken]!!.synonoyms[it]?:it}
            val inputScore = getCurrentScore(higherToken, tokenInput)
            if (inputScore >= getScoreThreshold(tokenInput, handlerMap[higherToken]!!.defaultMatchFraction)){
                // Passes threshold for matching handler for these words
                // Check if the required cogjects are present in nluworld
                val allCogjetRequirementsMet = handlerMap[higherToken]!!.requiredCogjectNames.all { requiredCogject -> nluWorldLine.hasValue(requiredCogject, time.toLong() + temporalOffset)}

                if (allCogjetRequirementsMet){
                    val handlerFunction = handlerMap[higherToken]!!.handler

                    val result = handlerFunction(handlerMap[higherToken]!!, tokenInput, nluWorldLine, time.toLong() + temporalOffset)
                    speechProcessingWorld?.addCogject(makeCogject(higherToken, result), time)
                    if (result in setOf(HANDLER_STATE.FAILURE, HANDLER_STATE.SUCCESS) ){
                        speechArgumentWorld?.expireKey(higherToken, time)
                        // Not expiring expiredHandlerName until it restarts processing speechProcessingWorld
                    }
                }
                else {
                    var canBeDeferred =false
                    for (requiredCogject in handlerMap[higherToken]!!.requiredCogjectNames){
                        val hasComputedResult =  nluWorldLine.hasValue(requiredCogject, time.toLong() + temporalOffset)
                        canBeDeferred = canBeDeferred || hasComputedResult
                        if (!canBeDeferred){
                            val processStateCogject = speechProcessingWorld!!.getState(time.toLong())[requiredCogject]
                            if (processStateCogject != null){
                                val processingState = processStateCogject.getValue<HANDLER_STATE>()
                                canBeDeferred = processingState in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.PROCESSING, HANDLER_STATE.WAITING)
                            }
                        }
                    }

                    if (canBeDeferred) {
                        speechProcessingWorld?.addValueCogject(higherToken, HANDLER_STATE.WAITING, time)
                        speechProcessingWorld?.addValueCogject("${higherToken}.speech-tokens", tokenInput, time)
                    }
                    else {
                        speechProcessingWorld?.addValueCogject(higherToken, HANDLER_STATE.FAILURE, time)
                    }
                }

            }
            else {
                // TODO: look ahead to decide if should allow this gap? (checking if higherToken in matchSet)
                speechProcessingWorld?.addValueCogject(higherToken, HANDLER_STATE.FAILURE, time)
            }
            // finally expire the token history for this value
            speechArgumentWorld?.expireKey(higherToken, time)
            return canonicalInput
        }
        // Main processing loop

        val lastIndex = annotatedTokens!!.size - 1
        for ((i, matchSet) in annotatedTokens!!.withIndex()){

            if (matchSet.size == 1){

                // matchSet.size == 1 if and only if the element in it is the raw speech token itself
                // None of the currently processing handlers match so expire them all (unless we allow skips)
                for (expiredHandlerName in speechProcessingWorld!!.getState(i).values.filter { cog -> cog.getValue() in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.WAITING)}.map { cog -> cog.name}){
                    val sWidth = handlerMap[expiredHandlerName]!!.maxMatchGapSize?:searchWidth
                    val cont = annotatedTokens!!.matchingIndexInRange(i, sWidth){ map -> map.contains(expiredHandlerName)}
                    if (cont == null)
                        processHigherTokenBoundary(expiredHandlerName, i)
                    else
                        speechArgumentWorld?.addCogject(makeCogject(expiredHandlerName, "*"), i)
                }
            }
            else {

                if (i > 0){
                    //
                    for (expiredHandlerName in speechProcessingWorld!!.getState(i).values.filter {cog -> cog.getValue() in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.WAITING) && !matchSet.contains(cog.name) }.map {it.name}){
                        val cont = annotatedTokens!!.matchingIndexInRange(i, searchWidth){ map -> map.contains(expiredHandlerName)}
                        if (cont == null)
                            processHigherTokenBoundary(expiredHandlerName, i)
                        else
                            speechArgumentWorld?.addCogject(makeCogject(expiredHandlerName, "*"), i)
                    }
                }

                for (token in matchSet){
                    val handler = handlerMap[token]
                    if (handler != null){
                        if (handler.parsePredicates.isEmpty() || handler.parsePredicates.any{it -> nluWorldLine.hasValue(it)}){
                            // For every higher order token, there should be
                            val higherOrderToken = token
                            if (!speechProcessingWorld!!.hasValue(higherOrderToken, i) && speechProcessingWorld!!.state.containsKey(higherOrderToken)){
                                // There is a previous value for this key, so expire it to define a new birthday and replace
                                // with BUILDING
                                speechProcessingWorld?.expireKey(higherOrderToken, i - 1)
                            }

                            // This is a higher level token.  Get the token that is an argument to it
                            val tokenForThisHandler = getTokenMappingToCanonicalToken(matchSet, handler)
                            speechArgumentWorld?.addCogject(makeCogject(higherOrderToken, tokenForThisHandler?:"*"), i)

                            speechProcessingWorld?.addCogject(makeCogject(higherOrderToken, HANDLER_STATE.BUILDING), i)
                        }


                    }

                }
            }

        }

        var t = lastIndex + 1
        var remainingHandlers = speechProcessingWorld!!.getState(t).values.filter { cog -> cog.getValue() in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.WAITING)}.map { cog -> cog.name}
        var size = speechProcessingWorld!!.state.size
        var first = true
        while (remainingHandlers.isNotEmpty() && (first || remainingHandlers.size < size)){
            for (expiredHandlerName in remainingHandlers){
                // No lookahead for the last character
                processHigherTokenBoundary(expiredHandlerName, t)
            }
            size = remainingHandlers.size
            t++
            first = false
            remainingHandlers = speechProcessingWorld!!.getState(t).values.filter { cog -> cog.getValue() in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.WAITING)}.map { cog -> cog.name}
        }

        return nluWorldLine.getState(t + temporalOffset).values.find { cog -> speechProcessingWorld!!.getCogjectValue<HANDLER_STATE>(cog.name,lastIndex ) == HANDLER_STATE.SUCCESS}
    }
}


fun processSpeech(speech:String, handlers:List<IntentHandler>, nluWorldLine:WorldLine, searchWidth:Int = 2): Cogject? {
    var phraseTokens = speech.split(' ')

    val objectSet = FiniteSet(400)

    val speechProcessingWorld = WorldLine(objectSet)

    val speechArgumentWorld = WorldLine(objectSet)

    fun buildIndex(handlers:List<IntentHandler>): MutableMap<String, MutableSet<String>> {

        fun addHandler(handler: IntentHandler, invertedIndex:MutableMap<String, MutableSet<String>>){
            handler.synonoyms.forEach{(spokenWord:String, _:String)->
                var prior = invertedIndex?.get(spokenWord)?: mutableSetOf()
                prior.add(handler.intentName)
                invertedIndex[spokenWord] = prior
            }
        }

        val invertedIndex = mutableMapOf<String, MutableSet<String>>()

        for (spec:IntentHandler in handlers){
            addHandler(spec, invertedIndex)
        }
        return invertedIndex
    }

    fun getTokenMappingToCanonicalToken(annotatedTokens:MutableSet<String>, handlerOfCanonicalToken:IntentHandler): String? {
        return annotatedTokens.find { token -> handlerOfCanonicalToken.synonoyms.containsKey(token)}
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

    fun getMatchScore(speech:List<String>, matchSpec:List<Map<String, Int>> = buildTokenOrderMapsList(speech, 3)): Float {
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

    val handlerMap = mapOf<String, IntentHandler>(*handlers.map {it -> it.intentName to it}.toTypedArray())

    val tokenInvertedIndex = buildIndex(handlers)

    fun expandTokenSet(tokenSet: Set<String>, invertedMap: Map<String, Set<String>>, maxLevels:Int = 100): MutableSet<String> {
        var set = mutableSetOf<String>().apply {addAll(tokenSet)}
        var next:MutableSet<String> = set
        var level = 0
        do{
            set = next
            next = mutableSetOf<String>()
            for (token in set){
                next.add(token)
                invertedMap[token]?.forEach{
                    higherHandler -> next.add(higherHandler)
                    handlerMap[higherHandler]?.predicates?.forEach {predToken -> next.add(predToken)}
                }

            }
            level++
        }while (set != next && level < maxLevels)
        return set
    }

    val annotatedTokens = phraseTokens.map { spokenToken -> expandTokenSet(setOf(spokenToken), tokenInvertedIndex)}

    fun getScoreThreshold(tokens:List<String>): Float {
        // Technical note #2: probably need to make this Intent specific but this is just a first
        // pass optimization that makes processing faster
        return (1/tokens.size).toFloat()
    }

    fun <T> Cogject.getValue(): T {
        return (this as ValueCogject).getValue() as T
    }

    fun getCurrentInputTokens(higherToken:String, time:Number):MutableList<String> {
        var tokenInput = mutableListOf<String>()
        for (tokenCogject in speechArgumentWorld.getAllValuesSinceBirth(higherToken, time)){
            val token = (tokenCogject as ValueCogject).getValue() as String
            // Consecutive higher order or predicate token names are squashed
            if (!(isPredicateToken(token) || isHigherToken(token)) || tokenInput.isEmpty() || (tokenInput.size > 0 && tokenInput[tokenInput.lastIndex] != token)){
                tokenInput.add(token)
            }
        }
        return tokenInput
    }

    fun getCurrentScore(higherToken:String, tokenInput:List<String>): Float {
        val handler = handlerMap[higherToken]!!
        val inputInCanonicalForm = tokenInput.map {it -> handlerMap[higherToken]!!.synonoyms[it]?:it}

        val canonicalInput = handler.phraseTokens
        // Technical note #1: this assumes a similar tolerance for word order mismatches between the speaker
        // and the IntentHandler
        if (inputInCanonicalForm.size <= canonicalInput.size){
            // this is interpreting the speech from the perspective of the IntentHandler
            // The IntentHandler expects a certain fuzzy sequence of tokens matches the spoken input
            // against this
            return getMatchScore(inputInCanonicalForm, buildTokenOrderMapsList(canonicalInput, handler.maxMatchGapSize?:searchWidth))
        }
        else {
            // This is interpreting the canonical phrase from the perspective of the speaker's spoken
            // input.  Techincally, the windowSize should be define based on the speaker's preference
            // instead of the IntentHandler's [maxMatchGapSize] but we are assuming similar tolerance
            // for mismatches as explained above
            return getMatchScore(canonicalInput, buildTokenOrderMapsList(inputInCanonicalForm, handler.maxMatchGapSize?:searchWidth))
        }

    }

    fun processHigherTokenBoundary(higherToken:String, time: Number): List<String> {
        var deferredSpeechInput:List<String>? = null
        if (speechProcessingWorld.getCogjectValue<HANDLER_STATE>(higherToken, time) == HANDLER_STATE.WAITING){
            deferredSpeechInput = speechProcessingWorld.getCogjectValue("${higherToken}.speech-tokens", time)
        }

        var tokenInput = mutableListOf<String>()
        if (deferredSpeechInput != null){
            tokenInput.addAll(deferredSpeechInput)
        }
        else {
            tokenInput = getCurrentInputTokens(higherToken, time)
        }

        // score the tokenInput
        val canonicalInput = tokenInput.map {it -> handlerMap[higherToken]!!.synonoyms[it]?:it}
//            val handler = handlerMap[higherToken]!!
//
//            val inputScore = getMatchScore(canonicalInput, buildTokenOrderMapsList(handler.phraseTokens, handler.maxMatchGapSize?:searchWidth))
        val inputScore = getCurrentScore(higherToken, tokenInput)
        if (inputScore >= getScoreThreshold(tokenInput)){
            // Passes threshold for matching handler for these words
            // Check if the required cogjects are present in nluworld
            val allCogjetRequirementsMet = handlerMap[higherToken]!!.requiredCogjectNames.all { requiredCogject -> nluWorldLine.hasValue(requiredCogject, time)}

            if (allCogjetRequirementsMet){
                val handlerFunction = handlerMap[higherToken]!!.handler

                val result = handlerFunction(handlerMap[higherToken]!!, tokenInput, nluWorldLine, time.toLong())
                speechProcessingWorld.addCogject(makeCogject(higherToken, result), time)
                if (result in setOf(HANDLER_STATE.FAILURE, HANDLER_STATE.SUCCESS) ){
                    speechArgumentWorld.expireKey(higherToken, time)
                    // Not expiring expiredHandlerName until it restarts processing speechProcessingWorld
                }
            }
            else {
                var canBeDeferred =false
                for (requiredCogject in handlerMap[higherToken]!!.requiredCogjectNames){
                    val hasComputedResult =  nluWorldLine.hasValue(requiredCogject, time)
                    canBeDeferred = canBeDeferred || hasComputedResult
                    if (!canBeDeferred){
                        val processStateCogject = speechProcessingWorld.getState(time.toLong())[requiredCogject]
                        if (processStateCogject != null){
                            val processingState = processStateCogject.getValue<HANDLER_STATE>()
                            canBeDeferred = processingState in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.PROCESSING, HANDLER_STATE.WAITING)
                        }
                    }
                }

                if (canBeDeferred) {
                    speechProcessingWorld.addValueCogject(higherToken, HANDLER_STATE.WAITING, time)
                    speechProcessingWorld.addValueCogject("${higherToken}.speech-tokens", tokenInput, time)
                }
                else {
                    speechProcessingWorld.addValueCogject(higherToken, HANDLER_STATE.FAILURE, time)
                }
            }

        }
        else {
            // TODO: look ahead to decide if should allow this gap? (checking if higherToken in matchSet)
            speechProcessingWorld.addValueCogject(higherToken, HANDLER_STATE.FAILURE, time)
        }
        // finally expire the token history for this value
        speechArgumentWorld.expireKey(higherToken, time)
        return canonicalInput
    }
    // Main processing loop

    val lastIndex = annotatedTokens.size - 1
    for ((i, matchSet) in annotatedTokens.withIndex()){

        if (matchSet.size == 1){
            // matchSet.size == 1 if and only if the element in it is the raw speech token itself
            // None of the currently processing handlers match so expire them all (unless we allow skips)
            for (expiredHandlerName in speechProcessingWorld.getState(i).values.filter { cog -> cog.getValue() in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.WAITING)}.map { cog -> cog.name}){
                val cont = annotatedTokens.matchingIndexInRange(i, searchWidth){ map -> map.contains(expiredHandlerName)}
                if (cont == null)
                    processHigherTokenBoundary(expiredHandlerName, i)
                else
                    speechArgumentWorld.addCogject(makeCogject(expiredHandlerName, "*"), i)
            }
        }
        else {
            val speechToken = phraseTokens[i]
            println("matching ${speechToken} -> ${matchSet.sorted()}")

            if (i > 0){
                //
                for (expiredHandlerName in speechProcessingWorld.getState(i).values.filter {cog -> cog.getValue() in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.WAITING) && !matchSet.contains(cog.name) }.map {it.name}){
                    val cont = annotatedTokens.matchingIndexInRange(i, searchWidth){ map -> map.contains(expiredHandlerName)}
                    if (cont == null)
                        processHigherTokenBoundary(expiredHandlerName, i)
                    else
                        speechArgumentWorld.addCogject(makeCogject(expiredHandlerName, "*"), i)
                }
            }

            for (token in matchSet){
                val handler = handlerMap[token]
                if (handler != null){
                    // For every higher order token, there should be
                    val higherOrderToken = token
                    if (!speechProcessingWorld.hasValue(higherOrderToken, i) && speechProcessingWorld.state.containsKey(higherOrderToken)){
                        // There is a previous value for this key, so expire it to define a new birthday and replace
                        // with BUILDING
                        speechProcessingWorld.expireKey(higherOrderToken, i - 1)
                    }

                    // This is a higher level token.  Get the token that is an argument to it
                    val tokenForThisHandler = getTokenMappingToCanonicalToken(matchSet, handler)
                    speechArgumentWorld.addCogject(makeCogject(higherOrderToken, tokenForThisHandler?:"*"), i)

                    speechProcessingWorld.addCogject(makeCogject(higherOrderToken, HANDLER_STATE.BUILDING), i)
                }

            }
        }

    }

    var t = lastIndex + 1
    var remainingHandlers = speechProcessingWorld.getState(t).values.filter { cog -> cog.getValue() in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.WAITING)}.map { cog -> cog.name}
    var size = speechProcessingWorld.state.size
    var first = true
    while (remainingHandlers.isNotEmpty() && (first || remainingHandlers.size < size)){
        for (expiredHandlerName in remainingHandlers){
            // No lookahead for the last character
            processHigherTokenBoundary(expiredHandlerName, t)
        }
        size = remainingHandlers.size
        t++
        first = false
        remainingHandlers = speechProcessingWorld.getState(t).values.filter { cog -> cog.getValue() in setOf(HANDLER_STATE.BUILDING, HANDLER_STATE.WAITING)}.map { cog -> cog.name}
    }

    return nluWorldLine.getState(t).values.find { cog -> speechProcessingWorld.getCogjectValue<HANDLER_STATE>(cog.name,lastIndex ) == HANDLER_STATE.SUCCESS}
}