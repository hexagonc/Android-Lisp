package com.evolved.automata.lisp
import kotlinx.coroutines.*

fun addFunctions(env: Environment) {
    env.mapFunction("test-coroutine", object: SimpleFunctionTemplate(){
        override fun evaluate(env: Environment?, evaluatedArgs: Array<out Value>?): Value {
            runBlocking {
                delay(3000)
                println("Hello")
            }

            return Environment.getNull()
        }

    })
}