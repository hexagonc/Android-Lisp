package com.evolved.automata

import com.evolved.automata.lisp.*
import com.evolved.automata.nn.util.UnsignedNumVectorType
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
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


            env.mapFunction("create-worldline", createWorld())
            env.mapFunction("get-cogject-value", getCogjectValue())
            env.mapFunction("create-cogject", createCogject(cache))


            // State cogject functions
            env.mapFunction("get-cogject-state", getStateNameFunction())
            env.mapFunction("get-cogject-state-duration", getStateDuration())
            env.mapFunction("get-cogject-name", getCogjectNameFunction())
            env.mapFunction("copy-cogject", getCogjectCopyFunction())
            env.mapFunction("State.get-all-states", getAllStateNamesFunction())


            env.mapFunction("worldline-set-value", worldlineSetValue())
            env.mapFunction("worldline-get-state", worldlineGetState())
            env.mapFunction("Worldline.process-time", worldlineProcessTime())

            env.mapFunction("debug", getPrintNameFunction())

            env.mapFunction("worldline-get-current-value", worldlineGetCurrentValue())

            env.mapFunction("worldline-get-last-update-spec", worldlineGetLastUpdateSpec())
            env.mapFunction("worldline-expire-key", worldlineExpireKey())
            env.mapFunction("worldline-pop-last-value", worldlinePopLastValue())
            env.mapFunction("worldline-get-update-times", worldlineGetLastUpdateTimes())


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

        fun addLispCogjectLambdas(env: Environment, cog: Cogject){
            env.mapFunction("this.cogject-name", getCogjectNameFunction(cog))
            env.mapFunction("this.cogject-copy", getCogjectCopyFunction(cog))
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

                addLispCogjectLambdas(innerEnv,StateMachineCogject@this )
                lispLambda.setActualParameters(arrayOf<Value>(ExtendedFunctions.makeValue(world), NLispTools.makeValue(time)))
                lispLambda.evaluate(innerEnv, false)

            }
            return Pair(name, lambda)
        }


        fun createCogject( cache: FiniteSet): SimpleFunctionTemplate {
            data class ArgModel(var cache:FiniteSet, val value: Value,val lambda: FunctionTemplate? = null)

            fun createSimpleValueCogject(evaluatedArgs: Array<out Value>): ArgModel {

                var myCache: FiniteSet = cache

                when (evaluatedArgs.size){
                    2 -> return ArgModel(myCache, evaluatedArgs[1])
                    3 -> if (evaluatedArgs[2].isLambda)
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
                            if (evaluatedArgs[1].isString){
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