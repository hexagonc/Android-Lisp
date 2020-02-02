package com.evolved.automata

import com.evolved.automata.lisp.*
import com.evolved.automata.nn.util.UnsignedNumVectorType
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import java.lang.RuntimeException



class WorldLineLispFunctions {

    companion object {
        fun addFunctions(env: Environment, cache: FiniteSet) {
            env.mapFunction("create-worldline", createWorld())
            env.mapFunction("get-cogject-value", getCogjectValue())
            env.mapFunction("create-cogject", createCogject(cache))
            env.mapFunction("worldline-set-value", worldlineSetValue())
            env.mapFunction("worldline-get-state", worldlineGetState())
            env.mapFunction("debug", getPrintNameFunction())

            env.mapFunction("worldline-get-current-value", worldlineGetCurrentValue())


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


        fun worldlineGetCurrentValue(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(3, false, false)
                    val world = evaluatedArgs[0].objectValue as WorldLine
                    val name = evaluatedArgs[1].string
                    val time = evaluatedArgs[2].intValue

                    val result = world.getLastValue(name, time)

                    return if (result != null) ExtendedFunctions.makeValue(result) else Environment.getNull()
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return worldlineGetCurrentValue() as T
                }

            }

        }

        fun worldlineGetState(): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>): Value {
                    checkActualArguments(2, false, false)
                    val world = evaluatedArgs[0].objectValue as WorldLine

                    val time = evaluatedArgs[1].intValue

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


        fun getSetStateFunction(time: Long, cog:StateMachineCogject): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {
                    val nextState = evaluatedArgs[0].string
                    cog.setNextState(nextState, time)
                    return evaluatedArgs[0]
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getSetStateFunction(time, cog) as T
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

        fun getStateDuration(cog:StateMachineCogject): SimpleFunctionTemplate {
            return object: SimpleFunctionTemplate(){
                override fun evaluate(env: Environment, evaluatedArgs: Array<out Value>): Value {

                    return NLispTools.makeValue(cog.lastStateTransition)
                }

                override fun <T :FunctionTemplate> innerClone(): T {
                    return getStateDuration(cog) as T
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

        fun getStateCogjectTransitionSpec(env: Environment, pair:Value): Pair<String, StateMachineCogject.(world: WorldLine, time:Long)->FloatArray> {
            val name = pair.list[0].string
            val lispLambda = pair.list[1].lambda


            var innerEnv = if (lispLambda is Lambda) lispLambda.innerEnvironment else Environment(env)
            val lambda:StateMachineCogject.(world: WorldLine, time:Long)->FloatArray = {world: WorldLine, time:Long->
                innerEnv.mapFunction("this.set-next-state", getSetStateFunction(time, StateMachineCogject@this))
                innerEnv.mapFunction("this.get-state-name", getStateNameFunction(StateMachineCogject@this))
                innerEnv.mapFunction("this.time-in-state", getStateDuration(StateMachineCogject@this))


                addLispCogjectLambdas(innerEnv,StateMachineCogject@this )
                lispLambda.setActualParameters(arrayOf<Value>(ExtendedFunctions.makeValue(world), NLispTools.makeValue(time)))
                lispLambda.evaluate(innerEnv, false)
                stateValue
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
                            if (evaluatedArgs[1].isList){

                                val stateSpecLisp:Array<Value> = evaluatedArgs[1].list

                                result = ExtendedFunctions.makeValue(StateMachineCogject(name,
                                        getStateCogjectTransitionSpec(env, stateSpecLisp[0]),
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