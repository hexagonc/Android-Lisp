package com.evolved.automata

import com.evolved.automata.nn.util.*
import kotlin.random.Random


private fun nop() = "Nothing"


open abstract class Cogject(val stateType: VectorType, var stateValue: FloatArray, var name: String = UNKNOWN_ITEM_NAME) {

    var processInterceptor: (Cogject.(WorldLine,Long) -> FloatArray)? = null

    var capabilities: MutableSet<Capability> = mutableSetOf()

    var favors = mutableListOf<Favor>()

    override fun toString(): String {
        return "Cogject: <${name?:"unknown"} ${stateType.stringConverter.toString(stateValue)}>"
    }

    fun addCapability(capability: Capability): Cogject {
        capabilities.add(capability)
        capability.context = this
        return this
    }


    abstract fun  copy(): Cogject


    open fun requestFavor(goal: Goal, world: WorldLine, time: Long, onFavorComplete: (FAVOR_STATUS)->Unit):Boolean {
        var foundCapability = false
        capabilities.forEach{cap ->
            if (cap.canTheoreticallyAchieveGoal(goal)) {

                val favor = cap.requestFavor(world = world, time = time, goalToAchieve = goal, onFinished = onFavorComplete)
                if (favor!=null)
                {
                    foundCapability = true
                    favors.add(favor)
                }
            }
        }

        return foundCapability
    }

    fun query(): Pair<Cogject, ((Cogject) -> Boolean)?> {
        return Pair(this, null)
    }


    fun getGoalConfidence(goal: Goal): Float {
        val capability = capabilities.find{cap -> cap.canTheoreticallyAchieveGoal(goal)}
        return capability?.getConfidence()?:0F
    }

    open fun process(world: WorldLine, processTime: Long) : FloatArray {
        val snap = processInterceptor
        stateValue =if (snap != null) snap(world, processTime) else stateValue
        favors.removeIf{favor: Favor -> val status = favor.process(world, processTime); status !in setOf<FAVOR_STATUS>(FAVOR_STATUS.WORKING, FAVOR_STATUS.PENDING)}
        return processInternal(world, processTime)
    }

    open fun processInternal(world: WorldLine, processTime: Long): FloatArray {
        return stateValue
    }

    open fun getBooleanIntValue(bool: Boolean): Float {
        return if (bool) 1F else 0F
    }

    fun interceptor(interceptor: (Cogject.(WorldLine,Long) -> FloatArray)): Cogject{
        processInterceptor = interceptor
        return this
    }

    override fun equals(other: Any?): Boolean {
        when {
            other is Cogject -> {
                nop();
                return other.name == name &&
                        other.stateType == stateType &&
                        other.stateValue.contentEquals(stateValue)}
            else -> return false
        }
    }
}



open class StateMachineCogject(name: String = UNKNOWN_ITEM_NAME,  private val initialState: Pair<String, StateMachineCogject.(WorldLine, Long) -> FloatArray>, private val stateSpecs: Array<Pair<String, (StateMachineCogject.(WorldLine,Long)->FloatArray)>>): Cogject(TallyVector(stateSpecs.size+1), FloatArray(stateSpecs.size+1){0F }, name) {

    val stringMap: StringToIntConversion

    var lastStateTransition: Long = -1L

    var prevStateName: String? = null

    var currentStateName: String = initialState.first

    init {
        stringMap = StringToIntConversion().also {me -> stateSpecs.forEach { nameLambdaPair -> me.addString(nameLambdaPair.first) }; me.addString(initialState.first)}

    }

    open fun onUpdatedValue(world: WorldLine, time: Long): Cogject {
        world.setValue(copy(), time+1)
        return this
    }

    override fun copy(): Cogject {
        var cog =  StateMachineCogject(initialState =initialState, stateSpecs = stateSpecs, name = name)
        cog.currentStateName = currentStateName
        cog.prevStateName = prevStateName
        cog.lastStateTransition = lastStateTransition
        return cog
    }

    override fun toString(): String {
        return "StateMachineCogject<$currentStateName>"
    }

    override fun process(world: WorldLine, processTime: Long): FloatArray {
        val stateIndex = stateType.vectorToValue(stateValue) as Int
        if (lastStateTransition == -1L)
            lastStateTransition = processTime
        stateValue = stateSpecs[stateIndex].second(this, world, processTime)
        if (currentStateName != prevStateName){
            onUpdatedValue(world, processTime)
        }
        return super.process(world, processTime)
    }

    /**
     * Only call this when the state changes
     */
    fun setNextState(name: String, time: Long): FloatArray {
        if (prevStateName != currentStateName){
            prevStateName = currentStateName
            lastStateTransition = time
        }

        currentStateName = name
        stateValue = (stateType as TallyVector).valueToVector(stringMap.getStringIndex(name)?:TODO("Need to decide what happens when setting an invalid state name"))
        return stateValue
    }


}


open class IntCogject(private val value: Int, private val size: Int, name: String = UNKNOWN_ITEM_NAME):Cogject(TallyVector(size), FloatArray(size){ i -> if (i >= value) 0F else 1F}, name ) {
    fun getValue(): Int {
        return (stateType as TallyVector).getValue(stateValue)
    }

    override fun copy(): Cogject {
        return IntCogject(value, size, name)
    }
}

open class RandomIntCogject(name: String):IntCogject(Random.nextInt(0, 10), 10, name)

open class SetCogject(private val items: Array<String>,private val set: Set<String>, name: String = UNKNOWN_ITEM_NAME) : Cogject(SetVector(items), FloatArray(items.size){ i -> if (set.contains(items[i])) 1F else 0F}, name) {

    fun getValue() : Set<String> {
        return HashSet<String>().apply{addAll((stateType as SetVector).getSet(stateValue).map{ it-> it as String})}
    }

    override fun copy(): Cogject {
        return SetCogject(items, set, name)
    }
}



abstract class ValueCogject(private val type: UnsignedNumVectorType,value: Any, name: String): Cogject(type, type.valueToVector(value), name) {
    abstract fun getValue(): Any


}