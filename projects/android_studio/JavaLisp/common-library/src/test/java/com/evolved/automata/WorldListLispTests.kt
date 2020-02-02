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

            var stateLispCogject = env.evaluate("(create-cogject \"robot\" (list (list \"initial\"  (lambda (world time)  (debug \"cogject name: \" (this.cogject-name)) (this.set-next-state \"final\"))) (list \"final\"  (lambda (world time)  (debug \"**finished** \")))))", true)

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

            var world = WorldLine()

            message = "Failed to add cogject to world"

            env.evaluate("(setq world (create-worldline))", false)

            env.evaluate("(worldline-set-value world 0 (create-cogject \"sonar\" 45))", false)

            env.evaluate("(debug (worldline-get-state world 0))", false)

            env.evaluate("(debug (worldline-get-current-value world \"sonar\" 4))", false)
        }
        catch (e: Throwable){
            e.printStackTrace()
            assertTrue(message, false)
        }
    }


}