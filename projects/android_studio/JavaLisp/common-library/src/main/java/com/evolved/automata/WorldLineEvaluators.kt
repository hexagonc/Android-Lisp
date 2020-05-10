package com.evolved.automata

import com.evolved.automata.lisp.*
import java.lang.RuntimeException



class WorldLineLispFunctions {

    companion object {
        fun addFunctions(env: Environment, cache: FiniteSet) {
            fun getStateNameFunction(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                        val cog = evaluatedArgs[0].objectValue as StateMachineCogject
                        return NLispTools.makeValue(cog.currentStateName)
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getStateNameFunction() as T
                    }

                }
            }

            fun getStateDuration(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                        val cog = evaluatedArgs[0].objectValue as StateMachineCogject
                        return NLispTools.makeValue(cog.lastStateTransition)
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getStateDuration() as T
                    }

                }
            }


            fun getCogjectNameFunction(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                        val cog = evaluatedArgs[0].objectValue as Cogject
                        return NLispTools.makeValue(cog.name)
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getCogjectNameFunction() as T
                    }

                }
            }

            fun getCogjectCopyFunction(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                        val cog = evaluatedArgs[0].objectValue as Cogject
                        return ExtendedFunctions.makeValue(cog.copy())
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getCogjectCopyFunction() as T
                    }

                }
            }

            fun getAllStateNamesFunction(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                        val cog = evaluatedArgs[0].objectValue as StateMachineCogject
                        var map = HashMap<String, Value>()

                        fun getValue(key: String): Value {
                            return NLispTools.makeValue(1)
                        }

                        return StringHashtableValue(cog.stringMap.getStrings().forEach{key->map.put(key, getValue(key))}.run {map})
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getAllStateNamesFunction() as T
                    }

                }
            }

            env.mapFunction("cogject-process", cogjectProcess())
            env.mapFunction("create-worldline", createWorld())
            env.mapFunction("get-cogject-value", getCogjectValue())
            env.mapFunction("create-cogject", createCogject(cache))

            env.mapFunction("is-cogject", isCogject())
            env.mapFunction("cogject-life-time", getLifeDuration())
            env.mapFunction("cogject-death-time", getDeathTime())


            // State cogject functions
            env.mapFunction("get-cogject-state", getStateNameFunction())
            env.mapFunction("get-cogject-state-duration", getStateDuration())
            env.mapFunction("get-cogject-name", getCogjectNameFunction())
            env.mapFunction("copy-cogject", getCogjectCopyFunction())
            env.mapFunction("State.get-all-states", getAllStateNamesFunction())


            env.mapFunction("worldline-has-value", worldlineHasValue())
            env.mapFunction("worldline-set-value", worldlineSetValue())
            env.mapFunction("worldline-get-state", worldlineGetState())
            env.mapFunction("worldline.process-time", worldlineProcessTime())

            env.mapFunction("debug", getPrintNameFunction())

            env.mapFunction("worldline-get-current-value", worldlineGetCurrentValue())

            env.mapFunction("worldline-get-last-update-spec", worldlineGetLastUpdateSpec())
            env.mapFunction("worldline-expire-key", worldlineExpireKey())
            env.mapFunction("worldline-pop-last-value", worldlinePopLastValue())
            env.mapFunction("worldline-get-update-times", worldlineGetLastUpdateTimes())

            env.mapFunction("worldline-get-ordered-keys", worldlineGetOrderedKeysAt())
            env.mapFunction("worldline-get-value-meta-data", worldlineGetKeyMetadataAt())
            env.mapFunction("worldline-get-all-values-at", worldlineGetAllValuesAt())

            env.mapFunction("worldline-get-universe-scale", worldlineGetWorldScale())
            env.mapFunction("worldline-delete-older-than world", worldlineClearAllOlderThan())

            env.mapFunction("worldline-serialize", worldlineSerialize())

            env.mapFunction("create-speech-intent", createSpeechIntent())
            env.mapFunction("create-speech-context", createSpeechContext())
            env.mapFunction("speech-process", speechProcess())
            env.mapFunction("speech-context-get-process-world", speechContextGetProcessWorld())
            env.mapFunction("speech-context-get-handlers", speechContextGetHandlers())
            env.mapFunction("speech-context-reset-model", speechContextResetModel())
            env.mapFunction("speech-context-clear-result-meta", speechContextResetResultMetaData())
            env.mapFunction("speech-context-reset", speechContextReset())

            env.mapFunction("speech-context-get-handler", speechContextGetHandler())
            env.mapFunction("speech-context-get-handlers-with-type", speechContextGetHandlersWithTypes())


            // Handler methods
            env.mapFunction("speech-handler-get-types", speechHandlerGetTypes())
            env.mapFunction("speech-handler-remove-type", speechHandlerRemoveType())
            env.mapFunction("speech-handler-add-type", speechHandlerAddType())
            env.mapFunction("speech-handler-add-synonym", speechHandlerAddSynonym())
            env.mapFunction("speech-handler-remove-synonym", speechHandlerRemoveSynonym())
            env.mapFunction("speech-handler-get-synonyms", speechHandlerGetSynonyms())



            // , (), (), ()
            // (), (), (), (), ()
            // ()
        }

        /**
         * (create-speech-intent {intent-name}
         *                       {speech-phrase}
         *                       {synonym-map}
         *                       {required-cogjects}
         *                       {output-cogject-names}
         *                       {type-list}
         *                       {search-error}
         *                       {handler-lambda}
         */
        fun createSpeechIntent(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                    var intentName:String? = null
                    var speech: String? = null
                    var synonymMap: MutableMap<String, String>? = null
                    var requiredCogjects:Set<String>? = null
                    var outputCogjects:Set<String>? = null
                    var types:Set<String>? = null
                    var searchError:Int? = null
                    var handler:(IntentHandler.(tokens:List<String>, outputWorldLine:WorldLine, time:Long) -> HANDLER_STATE)? = null

                    var key:String? = null
                    for ((i, arg) in evaluatedArgs.withIndex()){
                        if (key == null){
                            key = arg.string
                        }
                        else {
                            when (key) {
                                "intent-name" -> intentName = arg.string
                                "speech-phrase" -> speech = arg.string
                                "synonym-map" -> {
                                    if (!arg.isNull){
                                        synonymMap = mutableMapOf()
                                        for ((syn, value) in arg.stringHashtable.entries){
                                            synonymMap[syn] = value.string
                                        }
                                    }
                                }
                                "required-cogjects" -> if (!arg.isNull) {
                                    requiredCogjects = arg.list.map {v -> v.string}.toSet()
                                }
                                "output-cogject-names" -> if (!arg.isNull){
                                    outputCogjects = arg.list.map {v -> v.string}.toSet()
                                }
                                "type-list" -> if (!arg.isNull){
                                    types = arg.list.map {v -> v.string}.toSet()
                                }
                                "search-error" -> if (!arg.isNull){
                                    searchError = arg.intValue.toInt()
                                }
                                "handler-lambda" -> {
                                    val lambda = arg.lambda
                                    handler = {tokens:List<String>, outputWorldLine:WorldLine, time:Long->
                                        lambda.setActualParameters(arrayOf<Value>(ListValue(tokens.map{NLispTools.makeValue(it)}.toTypedArray()),ExtendedFunctions.makeValue(outputWorldLine), NLispTools.makeValue(time)))
                                        addSpeechIntentMethods(IntentHandler@this, (lambda as Lambda).innerEnvironment, time)

                                        var result:HANDLER_STATE = HANDLER_STATE.FAILURE

                                        fun setSuccess(): SimpleFunctionTemplate {
                                            return object: SimpleFunctionTemplate(){
                                                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                                                    result = HANDLER_STATE.SUCCESS
                                                    for (outputName in outputCogjectNames!!){
                                                        outputWorldLine.addValueCogject(outputName, evaluatedArgs[0], time)
                                                    }
                                                    return evaluatedArgs[0]
                                                }

                                                override fun <T :FunctionTemplate> innerClone(): T {
                                                    return setSuccess() as T
                                                }

                                            }
                                        }

                                        fun setFailure(): SimpleFunctionTemplate {
                                            return object: SimpleFunctionTemplate(){
                                                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                                                    result = HANDLER_STATE.FAILURE
                                                    return Environment.getNull()
                                                }

                                                override fun <T :FunctionTemplate> innerClone(): T {
                                                    return setFailure() as T
                                                }

                                            }
                                        }

                                        fun setWaiting(): SimpleFunctionTemplate {
                                            return object: SimpleFunctionTemplate(){
                                                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                                                    result = HANDLER_STATE.WAITING
                                                    return Environment.getNull()
                                                }

                                                override fun <T :FunctionTemplate> innerClone(): T {
                                                    return setWaiting() as T
                                                }

                                            }
                                        }

                                        env.mapFunction("intent-set-success", setSuccess())
                                        env.mapFunction("intent-set-failure", setFailure())
                                        env.mapFunction("intent-set-waiting", setWaiting())

                                        lambda.evaluate(env, false)
                                        result
                                    }
                                }
                            }
                            key = null
                        }
                    }

                    return ExtendedFunctions.makeValue(makeSpeechIntent(
                            intentTokenName = intentName!!,
                            phrase = speech!!,
                            synonymMap = synonymMap,
                            maxGapSize = searchError,
                            requiredCogjects = requiredCogjects,
                            outputCogjects = outputCogjects,
                            basePredicates = types,
                            handler = handler!!
                            ))
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return createSpeechIntent() as T
                }

            }

        }

        fun addSpeechIntentMethods(handler:IntentHandler, env:Environment, time: Long){
            fun addType(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                        handler.predicates.add(evaluatedArgs[0].string)
                        return Environment.getNull()
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return addType() as T
                    }

                }
            }

            fun removeType(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                        handler.predicates.remove(evaluatedArgs[0].string)
                        return Environment.getNull()
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return removeType() as T
                    }

                }
            }

            fun getTypes(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                        return ListValue(handler.predicates.map {it -> NLispTools.makeValue(it)}.toTypedArray())
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getTypes() as T
                    }

                }
            }

            fun getSynonyms(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                        return ListValue(handler.predicates.map {it -> NLispTools.makeValue(it)}.toTypedArray())
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getSynonyms() as T
                    }

                }
            }

            fun getRequiredCogjects(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                        return ListValue(handler.requiredCogjectNames.map {it -> NLispTools.makeValue(it)}.toTypedArray())
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getRequiredCogjects() as T
                    }

                }
            }

            fun getOutputCogjects(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                        return ListValue(handler.outputCogjectNames.map {it -> NLispTools.makeValue(it)}.toTypedArray())
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getOutputCogjects() as T
                    }

                }
            }

            fun getIntentName(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                        return NLispTools.makeValue(handler.intentName)
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getIntentName() as T
                    }
                }
            }

            fun getIntentPhrase(): SimpleFunctionTemplate {
                return object: SimpleFunctionTemplate(){
                    override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                        val builder = java.lang.StringBuilder()
                        handler.phraseTokens.forEach{builder.append(it)}
                        return NLispTools.makeValue(builder.toString())
                    }

                    override fun <T :FunctionTemplate> innerClone(): T {
                        return getIntentPhrase() as T
                    }
                }
            }

            env.mapFunction("intent-add-type", addType())
            env.mapFunction("intent-remove-type", removeType())
            env.mapFunction("intent-get-types", getTypes())
            env.mapFunction("intent-get-synonyms", getSynonyms())
            env.mapFunction("intent-get-required-cogjects", getRequiredCogjects())
            env.mapFunction("intent-get-output-cogjects", getOutputCogjects())
            env.mapFunction("intent-get-name", getIntentName())
            env.mapFunction("intent-get-phrase", getIntentPhrase())

        }

        // (create-speech-context {worldline}
        //                        {handlers}
        //                        {search-error}
        //                        {temporal-offset}
        //                        {speech-processing-world}
        //                        {speech-argument-world})
        //
        fun createSpeechContext(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                    var speechText:String? = null

                    if (!evaluatedArgs[0].isNull){
                        speechText = evaluatedArgs[0].string
                    }

                    var offset: Long? = null

                    var handlers:MutableList<IntentHandler>? = null
                    var searchWidth:Int? = null
                    var nluWorldLine:WorldLine? = null
                    var speechProcessingWorld:WorldLine? = null
                    var speechArgumentWorld:WorldLine? = null

                    var key:String? = null

                    for (arg in evaluatedArgs){
                        if (key == null){
                            key = arg.string
                        }
                        else {
                            when (key){
                                "worldline" -> nluWorldLine = arg.objectValue as WorldLine
                                "handlers" -> handlers = mutableListOf<IntentHandler>().apply {addAll(arg.list.map{value -> value.objectValue as IntentHandler})}
                                "search-error" -> searchWidth = arg.intValue.toInt()
                                "temporal-offset" -> offset = arg.intValue
                                "speech-processing-world" -> speechProcessingWorld = arg.objectValue as WorldLine
                                "speech-argument-world" -> speechProcessingWorld = arg.objectValue as WorldLine

                            }
                            key = null
                        }
                    }

                    return ExtendedFunctions.makeValue(SpeechContext(
                            handlers = handlers!!,
                            speech = speechText,
                            temporalOffset = offset?:nluWorldLine!!.getLastTime()?:0L,
                            searchWidth = searchWidth?:2,
                            nluWorldLine = nluWorldLine!!,
                            speechProcessingWorld = speechProcessingWorld,
                            speechArgumentWorld = speechArgumentWorld
                    ))
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return createSpeechContext() as T
                }

            }

        }


        fun worldlineSetValue(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(3, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine

                    val time = evaluatedArgs[1].intValue

                    return ListValue(evaluatedArgs.drop(2).map {cogject -> ExtendedFunctions.makeValue(world.setValue( cogject.objectValue as Cogject, time ))}.toTypedArray())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineSetValue() as T
                }

            }
        }


        /**
         * Rarely need to do this unless you are doing exception handling
         */
        fun speechContextResetResultMetaData(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val speechContext = evaluatedArgs[0].objectValue as SpeechContext
                    speechContext.resetSpeechResultState()
                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechContextResetResultMetaData() as T
                }

            }
        }



        fun speechContextResetModel(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val speechContext = evaluatedArgs[0].objectValue as SpeechContext
                    speechContext.rebuildSpeechModelIndexes()

                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechContextResetModel() as T
                }

            }
        }



        fun speechContextReset(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val speechContext = evaluatedArgs[0].objectValue as SpeechContext
                    speechContext.rebuildSpeechModelIndexes()
                    speechContext.resetSpeechResultState()

                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechContextReset() as T
                }
            }
        }



        fun speechContextGetHandlers(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val speechContext = evaluatedArgs[0].objectValue as SpeechContext


                    return ListValue(speechContext.handlers.map{ExtendedFunctions.makeValue(it)}.toTypedArray())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechContextGetHandlers() as T
                }
            }
        }

        fun speechContextGetHandlersWithTypes(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val speechContext = evaluatedArgs[0].objectValue as SpeechContext
                    val typeList = evaluatedArgs[1].list.map {it.string}


                    return ListValue(speechContext.handlers.filter {intent -> intent.predicates.containsAll(typeList)}.map {ExtendedFunctions.makeValue(it)}.toTypedArray())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechContextGetHandlersWithTypes() as T
                }
            }
        }

        fun speechContextGetHandler(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val speechContext = evaluatedArgs[0].objectValue as SpeechContext
                    val intentName = evaluatedArgs[1].string

                    return ExtendedFunctions.makeValue(speechContext.handlerMap[intentName])
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechContextGetHandler() as T
                }
            }
        }


        fun speechHandlerGetSynonyms(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val handler = evaluatedArgs[0].objectValue as IntentHandler

                    val inner = HashMap<String, Value>()
                    val out = StringHashtableValue(inner)
                    handler.synonoyms.entries.forEach{(key, value) -> inner[key] = NLispTools.makeValue(value)}
                    return out
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechHandlerGetSynonyms() as T
                }
            }
        }



        fun speechHandlerRemoveSynonym(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val handler = evaluatedArgs[0].objectValue as IntentHandler
                    val synonym = evaluatedArgs[1].string
                    handler.synonoyms.remove(synonym)

                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechHandlerRemoveSynonym() as T
                }
            }
        }


        fun speechHandlerAddSynonym(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(3, true, true)
                    val handler = evaluatedArgs[0].objectValue as IntentHandler
                    val synonym = evaluatedArgs[1].string
                    val canonical = evaluatedArgs[2].string
                    handler.synonoyms[synonym] = canonical

                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechHandlerAddSynonym() as T
                }
            }
        }



        fun speechHandlerAddType(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val handler = evaluatedArgs[0].objectValue as IntentHandler
                    val type = evaluatedArgs[1].string

                    handler.predicates.add(type)

                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechHandlerAddType() as T
                }
            }
        }


        fun speechHandlerRemoveType(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val handler = evaluatedArgs[0].objectValue as IntentHandler
                    val type = evaluatedArgs[1].string

                    handler.predicates.remove(type)

                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechHandlerRemoveType() as T
                }
            }
        }

        fun speechHandlerGetTypes(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val handler = evaluatedArgs[0].objectValue as IntentHandler

                    return ListValue(handler.predicates.map{ExtendedFunctions.makeValue(it)}.toTypedArray())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechHandlerGetTypes() as T
                }
            }
        }

        fun speechProcess(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(3, true, true)
                    val speechContext = evaluatedArgs[0].objectValue as SpeechContext
                    val speech = evaluatedArgs[1].string
                    val outputCogjectName = evaluatedArgs[2].string
                    var temporalOffset:Long
                    if (evaluatedArgs.size > 3)
                        temporalOffset = evaluatedArgs[3].intValue
                    else
                        temporalOffset = speechContext.nluWorldLine.getLastTime()?:0L

                    val output = speechContext.processSpeech(speech, temporalOffset)
                    val lastTime = speechContext.nluWorldLine.getLastTime()?:0
                    val topPatterns = mutableSetOf<String>()
                    var maxLength = 0
                    speechContext.speechArgumentWorld?.getState()?.keys?.filter { key ->
                        val value = speechContext.speechProcessingWorld?.getState()?.get(key) as ValueCogject

                        HANDLER_STATE.SUCCESS == value?.getValue() as HANDLER_STATE
                    }?. forEach{key ->
                        val len:Int = speechContext?.speechArgumentWorld?.getAllValuesSinceBirth(key, lastTime)?.size?:0
                        if (topPatterns.isEmpty() || maxLength <= len){
                            maxLength = len
                            topPatterns.add(key)
                        }
                    }

                    if (topPatterns.size == 1){
                        val output = speechContext.nluWorldLine.getCogjectValue<Value>(topPatterns.iterator().next(), lastTime)
                        speechContext.nluWorldLine.addValueCogject(outputCogjectName, output!!, lastTime)
                    }

                    return ListValue(topPatterns.map{NLispTools.makeValue(it)}.toTypedArray())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechProcess() as T
                }

            }
        }

        fun speechContextGetProcessWorld(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val speechContext = evaluatedArgs[0].objectValue as SpeechContext
                    val out = speechContext.speechProcessingWorld

                    return ExtendedFunctions.makeValue(out)
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return speechContextGetProcessWorld() as T
                }

            }
        }


        fun worldlineGetLastUpdateSpec(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(3, false, false)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val name = evaluatedArgs[1].string
                    val time = evaluatedArgs[2].intValue

                    val result = world.getLastEntry(name, time)

                    return if (result != null) ListValue(arrayOf<Value>(ExtendedFunctions.makeValue(result.entry), NLispTools.makeValue(result.updateTime))) else Environment.getNull()
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineGetLastUpdateSpec() as T
                }

            }

        }



        fun worldlineExpireKey(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(3, false, false)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val name = evaluatedArgs[1].string
                    val time = evaluatedArgs[2].intValue

                    val result = world.removeKey(name, time)

                    return NLispTools.makeValue(result)
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineExpireKey() as T
                }

            }
        }



        fun worldlineGetLastUpdateTimes(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine

                    val result:List<Long> = when (evaluatedArgs.size) {
                        1 ->  world.getUpdateTimes()
                        2 ->  world.getUpdateTimes(evaluatedArgs[1].string)
                        3 -> world.getUpdateTimes(evaluatedArgs[1].intValue, evaluatedArgs[2].intValue)
                        else -> world.getUpdateTimes(evaluatedArgs[1].string, evaluatedArgs[2].intValue, evaluatedArgs[3].intValue)
                    }

                    return ListValue(result.map { t -> NLispTools.makeValue(t)}.toTypedArray())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineGetLastUpdateTimes() as T
                }

            }
        }


        fun worldlineGetOrderedKeysAt(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val time = evaluatedArgs[1].intValue

                    val keys = world.getKeysInOldestUpdateOrder(time)

                    return ListValue(keys.map { NLispTools.makeValue(it) }.toTypedArray<Value>())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineGetOrderedKeysAt() as T
                }

            }
        }


        fun worldlineGetKeyMetadataAt(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(3, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val key = evaluatedArgs[1].string
                    val time = evaluatedArgs[2].intValue

                    val data = world.getCogjectMetaData(key, time)

                    val map = HashMap<String, Value>()

                    if (data != null) {
                        map.put("key", NLispTools.makeValue(data?.keyName?:""))
                        map.put("value", NLispTools.makeValue(data?.valString?:""))
                        map.put("value-age", if (data.valueLifeTime != null) NLispTools.makeValue(data.valueLifeTime) else Environment.getNull())
                        map.put("total-age", if (data.totalLifeTime != null) NLispTools.makeValue(data.totalLifeTime) else Environment.getNull())
                        map.put("birthday-str", if (data.birthdayString != null) NLispTools.makeValue(data.birthdayString) else Environment.getNull())
                        map.put("death-time", if (data.deathDay != null) NLispTools.makeValue(data.deathDay!!) else Environment.getNull())
                    }

                    return StringHashtableValue(map)
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineGetKeyMetadataAt() as T
                }

            }
        }


        fun worldlineGetAllValuesAt(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(3, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val key = evaluatedArgs[1].string
                    val time = evaluatedArgs[2].intValue

                    return ListValue(world.getAllValuesAt(key, time).map { it -> ExtendedFunctions.makeValue(it) }.toTypedArray<Value>())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineGetAllValuesAt() as T
                }

            }
        }


        // (worldline-get-key-universe-scale world)
        // returns a list ({minimum-time} {maximum-time} {all-keys-ever-seen} )
        // worldlineGetWorldScale
        fun worldlineGetWorldScale(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine

                    val v1 = ListValue(world.everyKeyEveryCreated().map { it -> ExtendedFunctions.makeValue(it) }.toTypedArray<Value>())

                    val (earliest, latest) = Pair(world.getEarliestTime(), world.getLastTime())

                    return ListValue(
                            arrayOf(
                                    if (earliest != null) NLispTools.makeValue(earliest) else Environment.getNull(),
                            if (latest != null) NLispTools.makeValue(latest) else Environment.getNull(),v1))
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineGetWorldScale() as T
                }

            }
        }

        fun worldlineClearAllOlderThan(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val cutoffAge =evaluatedArgs[1].intValue

                    world.clearAllTransactionsInRange(world.getEarliestTime()?:0, cutoffAge)
                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineClearAllOlderThan() as T
                }

            }
        }


        fun worldlinePopLastValue(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val name = evaluatedArgs[1].string

                    val time = if (evaluatedArgs.size < 3) world.getLastTime() else evaluatedArgs[2].intValue

                    if (time == null)
                        return Environment.getNull()

                    val result = world.removeLatestValue(name, time)

                    return if (result != null) ExtendedFunctions.makeValue(result) else Environment.getNull()
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlinePopLastValue() as T
                }

            }
        }

        fun worldlineGetCurrentValue(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val name = evaluatedArgs[1].string


                    val time = if (evaluatedArgs.size < 3) world.getLastTime() else evaluatedArgs[2].intValue

                    if (time == null)
                        return Environment.getNull()

                    val result = world.getLastValue(name, time)

                    return if (result != null) ExtendedFunctions.makeValue(result) else Environment.getNull()
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineGetCurrentValue() as T
                }

            }
        }


        fun worldlineProcessTime(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine

                    val time = evaluatedArgs[1].intValue

                    if (evaluatedArgs.size > 2) {
                        world.process(evaluatedArgs.drop(2).map {v -> v.string}, time)
                    }
                    else
                        world.process(time)

                    val state = world.getState(time)
                    var out = HashMap<String, Value>()

                    return state.entries.forEach {entry -> out.put(entry.key, ExtendedFunctions.makeValue(entry.value))}.run {
                        StringHashtableValue(out)
                    }
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineProcessTime() as T
                }

            }

        }

        fun worldlineGetState(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine

                    val time = if (evaluatedArgs.size < 2) world.getLastTime() else evaluatedArgs[1].intValue

                    if (time == null)
                        return Environment.getNull()

                    val state = world.getState(time)
                    var out = HashMap<String, Value>()

                    return state.entries.forEach {entry -> out.put(entry.key, ExtendedFunctions.makeValue(entry.value))}.run {
                        StringHashtableValue(out)
                    }
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineGetState() as T
                }

            }

        }

        fun worldlineHasValue(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val key = evaluatedArgs[1].string
                    val time = if (evaluatedArgs.size < 3) world.getLastTime() else evaluatedArgs[2].intValue

                    if (time == null)
                        return Environment.getNull()

                    return NLispTools.makeValue(world.hasValue(key, time))
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineHasValue() as T
                }

            }

        }


        fun worldlineSerialize(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    var prevActiveKeys: Set<String>? = null
                    var worldSpecList = ArrayList<Value>()
                    for (time in world.getUpdateTimes()) {
                        val activeKeys = world.getActiveKeys(time)
                        prevActiveKeys?.forEach{key ->
                            if (!activeKeys.contains(key)){
                                worldSpecList.add(ListValue(arrayOf(NLispTools.makeValue(time), NLispTools.makeValue(key), Environment.getNull(), Environment.getNull(),  Environment.getNull(), NLispTools.makeValue(time))))
                            }
                        }

                        prevActiveKeys = activeKeys


                        for (key in activeKeys){

                            val valuesAtThisTime = world.getAllValuesAt(key, time)

                            valuesAtThisTime.forEach{cogject ->

                                when (cogject){
                                    is StateMachineCogject -> {
                                        val states = cogject.getAllStateNames()

                                        worldSpecList.add(ListValue(arrayOf(NLispTools.makeValue(time), NLispTools.makeValue(key), NLispTools.makeValue(cogject.currentStateName), ListValue(states.map{NLispTools.makeValue(it)}.toTypedArray()), Environment.getNull(), Environment.getNull())))}
                                    is ValueCogject -> {worldSpecList.add(ListValue(arrayOf(NLispTools.makeValue(time), NLispTools.makeValue(key), Environment.getNull(), Environment.getNull(),  cogject.getValue() as Value, Environment.getNull()))) }
                                }

                            }

                        }
                    }

                    val listSpec = ListValue(worldSpecList.toTypedArray()).serializedForm()
                    return NLispTools.makeValue("(build-world ${listSpec})")
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineSerialize() as T
                }

            }

        }


        fun getCogjectValue(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>?): Value {
                    checkActualArguments(1, false, false)
                    val cogject = evaluatedArgs!![0].objectValue as Cogject
                    if (cogject is ValueCogject) {
                        return cogject.getValue() as Value
                    }
                    else if (cogject is StateMachineCogject){
                        return NLispTools.makeValue((cogject).currentStateName)
                    }
                    throw RuntimeException("Unsupported cogject type $cogject")
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getCogjectValue() as T
                }

            }

        }


        fun cogjectProcess(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(3, false, false)
                    val cogject = evaluatedArgs!![0].objectValue as Cogject
                    val world = evaluatedArgs[1].objectValue as WorldLine
                    val time = evaluatedArgs[2].intValue

                    cogject.process(world, time)
                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return cogjectProcess() as T
                }

            }

        }

        fun createWorld(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>?): Value {

                    return ExtendedFunctions.makeValue(WorldLine())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return createWorld() as T
                }

            }

        }


        fun isCogject(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, false, false)

                    val obj = evaluatedArgs[0]
                    if  (obj.objectValue == null || obj.objectValue !is Cogject)
                        return Environment.getNull()
                    if (obj.objectValue is StateMachineCogject)
                        return NLispTools.makeValue("STATE_MACHINE_COGJECT")
                    else
                        return NLispTools.makeValue("VALUE_COGJECT")
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return isCogject() as T
                }

            }

        }


        fun getSetStateFunction(time: Long, cog:StateMachineCogject, world: WorldLine): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                    val nextState = evaluatedArgs[0].string
                    cog.setNextState(world, nextState, time)
                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getSetStateFunction(time, cog, world) as T
                }

            }
        }

        fun getStateNameFunction(cog:StateMachineCogject): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                    return NLispTools.makeValue(cog.currentStateName)
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getStateNameFunction(cog) as T
                }

            }
        }

        fun getAllStateNamesFunction(cog:StateMachineCogject): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                    var map = HashMap<String, Value>()

                    fun getValue(key: String): Value {
                        return NLispTools.makeValue(1)
                    }

                    return StringHashtableValue(cog.stringMap.getStrings().forEach{key->map.put(key, getValue(key))}.run {map})
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getAllStateNamesFunction(cog) as T
                }

            }
        }

        fun getStateDuration(cog:StateMachineCogject, time:Long): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                    return NLispTools.makeValue(time - cog.lastStateTransition)
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getStateDuration(cog, time) as T
                }

            }
        }

        fun getLifeDuration(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                    var world = evaluatedArgs[0].objectValue as WorldLine

                    val name:String = if (evaluatedArgs[1].isString) evaluatedArgs[1].string else (evaluatedArgs[1].objectValue as Cogject).name
                    val time: Long = evaluatedArgs[2].intValue
                    return NLispTools.makeValue(world.getCogjectCurrentAge(name, time))
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getLifeDuration() as T
                }

            }
        }

        fun getDeathTime(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                    var world = evaluatedArgs[0].objectValue as WorldLine
                    val cog: Cogject = evaluatedArgs[1].objectValue as Cogject
                    val time: Long = evaluatedArgs[2].intValue


                    val deathTime = world.getDeathTime(cog.name, time);
                    if (deathTime != null)
                        return NLispTools.makeValue(deathTime)
                    else
                        return Environment.getNull()
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getDeathTime() as T
                }

            }
        }


        fun getLifeDuration(cog:Cogject, time:Long): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                    var world = evaluatedArgs[0].objectValue as WorldLine
                    return NLispTools.makeValue(world.getCogjectCurrentAge(cog.name, time))
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getLifeDuration(cog, time) as T
                }

            }
        }

        fun getDeathTime(cog:Cogject, time:Long): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                    var world = evaluatedArgs[0].objectValue as WorldLine
                    val deathTime = world.getDeathTime(cog.name, time);
                    if (deathTime != null)
                        return NLispTools.makeValue(deathTime)
                    else
                        return Environment.getNull()
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getDeathTime(cog, time) as T
                }

            }
        }

        fun getCogjectNameFunction(cog:Cogject): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                    return NLispTools.makeValue(cog.name)
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getCogjectNameFunction(cog) as T
                }

            }
        }

        fun getCogjectCopyFunction(cog:Cogject): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                    return ExtendedFunctions.makeValue(cog.copy())
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getCogjectCopyFunction(cog) as T
                }

            }
        }

        fun addLispCogjectLambdas(env: Environment, cog: Cogject, time:Long){
            env.mapFunction("this.cogject-name", getCogjectNameFunction(cog))
            env.mapFunction("this.cogject-copy", getCogjectCopyFunction(cog))
            env.mapFunction("this.life-time", getLifeDuration(cog, time))
            env.mapFunction("this.death-time", getDeathTime(cog, time))
        }

        fun addLispWorldLineLambdas(env: Environment, cog: Cogject){

        }

        fun getStateCogjectTransitionSpec(env: Environment, pair:Value): Pair<String, StateMachineCogject.(world: WorldLine, time:Long)->Unit> {
            val name = pair.list[0].string
            val lispLambda = pair.list[1].lambda


            var innerEnv = if (lispLambda is Lambda) lispLambda.innerEnvironment else Environment(env)
            val lambda:StateMachineCogject.(world: WorldLine, time:Long)->Unit = {world: WorldLine, time:Long->
                innerEnv.mapFunction("this.set-next-state", getSetStateFunction(time, StateMachineCogject@this, world))
                innerEnv.mapFunction("this.get-state-name", getStateNameFunction(StateMachineCogject@this))
                innerEnv.mapFunction("this.time-in-state", getStateDuration(StateMachineCogject@this, time))


                innerEnv.mapFunction("state.get-all-states", getAllStateNamesFunction(StateMachineCogject@this))

                addLispCogjectLambdas(innerEnv,StateMachineCogject@this, time )
                lispLambda.setActualParameters(arrayOf<Value>(ExtendedFunctions.makeValue(world), NLispTools.makeValue(time)))
                lispLambda.evaluate(innerEnv, false)

            }
            return Pair(name, lambda)
        }


        fun createCogject( cache: FiniteSet): SimpleFunctionTemplate {
            data class ArgModel(var cache:FiniteSet, val value: Value,val lambda: FunctionTemplate? = null)

            fun createSimpleValueCogject(evaluatedArgs: Array<out Value>): ArgModel {

                var myCache: FiniteSet = cache

                when {
                    evaluatedArgs.size == 2 || evaluatedArgs.size == 3 && evaluatedArgs[2].isNull -> return ArgModel(myCache, evaluatedArgs[1])
                    evaluatedArgs.size == 3 -> if (evaluatedArgs[2].isLambda)
                                                    return ArgModel(myCache, evaluatedArgs[1], (evaluatedArgs[2] as LambdaValue).lambda)
                                                 else
                                                    return ArgModel(evaluatedArgs[2].objectValue as FiniteSet, evaluatedArgs[1])
                }
                throw RuntimeException("Invalid arguments for creating value lisp cogject: $evaluatedArgs")
            }

            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>?): Value {

                    var result: Value? = null

                    if (evaluatedArgs != null && evaluatedArgs.size > 1) {
                        val name = evaluatedArgs[0].string
                        var myCache: FiniteSet?
                        if (name != null){
                            if (evaluatedArgs[1].isString && evaluatedArgs.size>2 && evaluatedArgs[2].isList ){
                                val initialStateName = evaluatedArgs[1].string
                                val stateSpecLisp:Array<Value> = evaluatedArgs[2].list

                                result = ExtendedFunctions.makeValue(StateMachineCogject(name,
                                        initialStateName,
                                        stateSpecLisp.map { pair -> getStateCogjectTransitionSpec(env, pair)}.toTypedArray()
                                        ))
                            }
                            else {
                                val spec = createSimpleValueCogject(evaluatedArgs)

                                if (spec.lambda == null){
                                    val cog = spec.cache.makeValueCogject(name, spec.value) as Cogject?
                                    if (cog != null){
                                        result = ExtendedFunctions.makeValue(cog)
                                    }
                                    else {
                                        // This means the object couldn't be allocated
                                    }
                                }
                                else {
                                    val lambda:Cogject.(world: WorldLine, time:Long)->FloatArray = { world: WorldLine, time: Long ->
                                        spec.lambda.setActualParameters(arrayOf<Value>(ExtendedFunctions.makeValue(world), NLispTools.makeValue(time)))
                                        addLispCogjectLambdas((spec.lambda as Lambda).innerEnvironment,Cogject@this, time )

                                        spec.lambda.evaluate(env, false)

                                        stateValue
                                    }
                                    val cog = spec.cache.makeValueCogject(name, spec.value, lambda) as Cogject?
                                    if (cog != null){
                                        result = ExtendedFunctions.makeValue(cog)
                                    }
                                    else {
                                        // This means the object couldn't be allocated
                                    }

                                }

                            }
                        }
                        else {
                            throw RuntimeException("create-object: first argument must be String")
                        }


                    }
                    return if (result != null) result else Environment.getNull()
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return createCogject(cache) as T
                }

            }

        }

        fun getPrintNameFunction(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(1, true, true);

                    var sBuilder = StringBuilder()
                    for (i in 0..evaluatedArgs.size - 1)
                    {
                        sBuilder.append(if (evaluatedArgs[i].isString()) evaluatedArgs[i].getString() else evaluatedArgs[i].toString())
                    }
                    var out = sBuilder.toString();
                    println("$out\n")
                    return NLispTools.makeValue(out)
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getPrintNameFunction() as T
                }

            }
        }


    }






}