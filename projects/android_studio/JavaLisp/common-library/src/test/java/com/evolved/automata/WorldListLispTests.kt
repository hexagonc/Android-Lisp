package com.evolved.automata

import com.evolved.automata.lisp.*
import org.hamcrest.CoreMatchers.*
import org.junit.Assert.assertThat
import org.junit.Assert.assertTrue
import org.junit.Test


class WorldListLispTests {



    @Test fun testCreateWorldLineAndSimpleCogjects(){
        var message = "Failed to create WorldLine Value"
        try
        {
            var env = Environment()

            var cache = FiniteSet(2000)
            NLispTools.addDefaultFunctionsAddMacros(env)
            ExtendedFunctions.addExtendedFunctions(env)
            WorldLineLispFunctions.addFunctions(env, cache)

            fun evaluateGlobal(): FunctionTemplate {
                return object : FunctionTemplate() {
                    override fun clone(): Any? {
                        return evaluateGlobal()
                    }


                    @Throws(InstantiationException::class, IllegalAccessException::class)
                    override fun evaluate(e: Environment, resume: Boolean): Value {
                        if (!resume) {
                            resetFunctionTemplate()
                        }

                        var result = Environment.getNull()
                        while (_instructionPointer < _actualParameters.size) {
                            if (resume && _lastFunctionReturn.continuingFunction != null) {
                                _lastFunctionReturn = _lastFunctionReturn.continuingFunction.evaluate(env, resume)
                                result = _lastFunctionReturn
                            } else {
                                _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false)
                                result = _lastFunctionReturn
                            }

                            if (result.isContinuation)
                                return continuationReturn(result)
                            if (result.isBreak || result.isReturn || result.isSignal || result.isSignalOut)
                                return resetReturn(result)
                            _instructionPointer++
                        }

                        return resetReturn(result)

                    }
                }
            }

            env.mapFunction("global", evaluateGlobal())





            var timelineValue: Value = env.evaluate("(create-worldline)", true)

            assertThat(timelineValue, `is`(notNullValue()))

            println("Timeline value: $timelineValue")

            var expr = "(create-cogject \"sonar\" 45)"

            val simpleValueCogject = env.evaluate(expr, false)

            assertThat(simpleValueCogject, `is`(notNullValue()))


            assertTrue(message, !simpleValueCogject.isNull)

            println("Value: $simpleValueCogject")

            env.mapValue("cogject", simpleValueCogject)

            message = "Wrong lisp cogject value"

            assertTrue(message, simpleValueCogject.objectValue is ValueCogject)

            var embeddedCogject = simpleValueCogject.objectValue as ValueCogject

            message = "wrong cogject return value"
            assertTrue(message, embeddedCogject.getValue() is Value)

            expr = "(get-cogject-value cogject)"

            var valueCogject = env.evaluate(expr, false)

            message = "Failed to retrieve correct value"
            assertTrue(message, valueCogject != null && valueCogject.isInteger)

            println("cogject lisp value $valueCogject")

            expr = "(setq cogject (create-cogject \"sonar\" 12 (lambda (world time) (break) (global (setq done 1)))))"

            val lambdaResult = env.evaluate(expr, true)

            embeddedCogject = lambdaResult.objectValue as ValueCogject

            message = "Failed to evaluate cogject"

            embeddedCogject.process(WorldLine(), 0)

            message = "Failed to execute cogject process"
            assertTrue(message, env.hasVariable("done"))

            println("Result value is ${env.getVariableValue("done")}")


        }
        catch (e: Throwable) {
            e.printStackTrace()
            assertTrue(message, false)
        }
    }

    @Test
    fun testStatemachineLispCogject(){
        var message = "failed to create state machine cogject"

        try {
            var env = Environment()

            var cache = FiniteSet(2000)
            NLispTools.addDefaultFunctionsAddMacros(env)
            ExtendedFunctions.addExtendedFunctions(env)
            WorldLineLispFunctions.addFunctions(env, cache)

            env.evaluate("(debug \"hello, world!\")", true)

            var stateLispCogject = env.evaluate("(setq robot (create-cogject \"robot\" (list (list \"initial\"  (lambda (world time)  (debug \"cogject name: \" (this.cogject-name) \" and states: \" (state.get-all-states)) (this.set-next-state \"final\"))) (list \"final\"  (lambda (world time)  (debug \"**finished** \"))))))", true)

            message = "Failed to evaluate state cogject"

            var world = WorldLine()

            message = "Invalid state cogject"
            assertTrue(message, stateLispCogject!=null && !stateLispCogject.isNull)

            message = "wrong cogject type"
            assertTrue(message, stateLispCogject.objectValue is StateMachineCogject)

            println("State: $stateLispCogject")

            message = "failed to process cogject"

            var stateCogject = stateLispCogject.objectValue as StateMachineCogject

            stateCogject.process(world, 1)

            println("State: $stateLispCogject")

            var state = stateCogject.currentStateName

            message = "wrong next state: $state"

            assertTrue(message, state == "final")

            for (i in 2..10){
                stateCogject.process(world, i.toLong())
            }

            println("world state: ${world.getState(34)}")


            env.evaluate("(debug (State.get-all-states robot))", true)

        }
        catch (e: Throwable){
            e.printStackTrace()
            assertTrue(message, false)
        }
    }


    @Test
    fun testWorldLineFunctions(){
        var message = "failed to create state machine cogject"

        try {
            var env = Environment()

            var cache = FiniteSet(2000)
            NLispTools.addDefaultFunctionsAddMacros(env)
            ExtendedFunctions.addExtendedFunctions(env)
            WorldLineLispFunctions.addFunctions(env, cache)



            message = "Failed to add cogject to world"

            env.evaluate("(setq world (create-worldline))", false)

            env.evaluate("(worldline-set-value world 0 (create-cogject \"sonar\" 45))", false)

            env.evaluate("(debug (worldline-get-state world 0))", false)

            env.evaluate("(debug (worldline-get-current-value world \"sonar\" 4))", false)

            env.evaluate("(debug (worldline-get-last-update-spec world \"sonar\" 4))", false)

            var world = env.getVariableValue("world").objectValue as WorldLine
            env.evaluate("(worldline-expire-key world \"sonar\" 10)", false)

            message = "Failed to expire sonar"

            assertTrue(message, world.hasValue("sonar", 9) && !world.hasValue("sonar",10))

            env.evaluate("(debug (setq removed (worldline-pop-last-value world \"sonar\" 6)))", false)

            message = "Failed to remove sonar"

            assertTrue(message, !world.hasValue("sonar", 0))

            assertTrue(message, !world.hasValue("sonar", 89))


            env.evaluate("(for x (list (25 3) (28 6) (35 15) (50 20)) F (worldline-set-value world (second x) (create-cogject \"sonar\" (first x)) ))", false)

            println("${world.getUpdateTimes().map { t -> world.getState(t)}}\n")

            env.evaluate("(worldline-set-value world 5 (create-cogject \"bump\" 1))", false)
            env.evaluate("(debug (worldline-get-update-times world))", true)

            env.evaluate("(debug (worldline-get-update-times world \"sonar\"))", true)

            env.evaluate("(debug (worldline-get-update-times world \"sonar\" 2 89))", true)



        }
        catch (e: Throwable){
            e.printStackTrace()
            assertTrue(message, false)
        }
    }

    @Test
    fun testStateMachinesDetailed(){
        var message = ""
        try {
            var world = WorldLine()

            world.setValue(StateMachineCogject(
                    "robot",
                    Pair<String, StateMachineCogject.(WorldLine, Long) -> Unit>("initializing", {line: WorldLine, t: Long ->
                        fun goto(s: String) = setNextState(s, t)

                        println("starting robot\n")
                        goto("Working")
                        }),
                    arrayOf(Pair<String, StateMachineCogject.(WorldLine, Long) -> Unit>("Working", {line: WorldLine, t: Long ->
                        fun goto(s: String) = setNextState(s, t)
                        fun duration() = t - lastStateTransition
                        println("${t - lastStateTransition}")
                        if (duration()> 10000) {
                            println("Done")
                            goto("finished")
                        }
                    }),
                            Pair<String, StateMachineCogject.(WorldLine, Long) -> Unit>("finished", {line: WorldLine, t: Long ->
                                fun finish () = world.removeKey(name, t)

                                println("Done!!\n")
                                finish()

                            }))),
                    1)

            var time = System.currentTimeMillis()
            val stop = 15000 + time
            var timeout = time
            while (time < stop){
                world.process(time)
                Thread.sleep(100)
                if (time - timeout > 1000L) {
                    println("Working")
                    timeout = time
                }
                time = System.currentTimeMillis()

            }
        }
        catch (e: Throwable){
            e.printStackTrace()
            assertTrue(message, false)
        }


    }



}