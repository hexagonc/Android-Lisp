package com.evolved.automata

import com.evolved.automata.nn.util.UnsignedNumVectorType


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

                override fun processInternal(world: WorldLine, processTime: Long): FloatArray {
                    if (action == null)
                        return super.processInternal(world, processTime)
                    else
                        return action(world, processTime)
                }
            }
        }
        else {
            return null
        }
    }
}