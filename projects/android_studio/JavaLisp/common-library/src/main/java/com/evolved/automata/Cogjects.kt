package com.evolved.automata

import com.evolved.automata.nn.util.*
import kotlin.random.Random


private fun nop() = "Nothing"


open abstract class Cogject(val stateType: VectorType, var name: String = UNKNOWN_ITEM_NAME) {

    var processInterceptor: (Cogject.(WorldLine,Long) -> Unit)? = null

    var capabilities: MutableSet<Capability> = mutableSetOf()

    var favors = mutableListOf<Favor>()

    abstract val stateValue: FloatArray

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
        if (snap != null)
            snap(world, processTime)
        favors.removeIf{favor: Favor -> val status = favor.process(world, processTime); status !in setOf<FAVOR_STATUS>(FAVOR_STATUS.WORKING, FAVOR_STATUS.PENDING)}
        processInternal(world, processTime)
        return stateValue
    }

    open fun processInternal(world: WorldLine, processTime: Long): Unit {

    }

    open fun getBooleanIntValue(bool: Boolean): Float {
        return if (bool) 1F else 0F
    }

    fun interceptor(interceptor: (Cogject.(WorldLine,Long) -> Unit)): Cogject{
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



open class StateMachineCogject(name: String = UNKNOWN_ITEM_NAME,  initialStateName: String, val stateSpecs: Array<Pair<String, (StateMachineCogject.(WorldLine,Long)->Unit)>>): Cogject(TallyVector(stateSpecs.size), name) {

    val stringMap: StringToIntConversion

    var lastStateTransition: Long = -1L

    var currentStateName: String = initialStateName

    override val stateValue: FloatArray

    init {
        stringMap = StringToIntConversion().also {me -> stateSpecs.forEach { nameLambdaPair -> me.addString(nameLambdaPair.first) }}
        stateValue = stateType.valueToVector(stringMap.getStringIndex(currentStateName))
    }

    open fun onUpdatedValue(world: WorldLine, time: Long): Cogject {

        return this
    }

    override fun copy(): Cogject {
        var cog =  StateMachineCogject(initialStateName = currentStateName, stateSpecs = stateSpecs, name = name)
        return cog
    }

    override fun toString(): String {
        return "StateMachineCogject<$currentStateName>"
    }

    override fun processInternal(world: WorldLine, processTime: Long): Unit {
        val stateIndex = stateType.vectorToValue(stateValue) as Int
        if (lastStateTransition == -1L)
            lastStateTransition = processTime

        stateSpecs[stateIndex].second(this, world, processTime)
    }

    /**
     * Only call this when the state changes
     */
    fun setNextState(w: WorldLine, name: String, time: Long): Boolean {
        if (currentStateName != name){
            w.setValue(StateMachineCogject(name = this.name, stateSpecs = stateSpecs, initialStateName = name), time+1)
            return true
        }

        return false
    }


}


open class IntCogject(private val value: Int, private val size: Int, name: String = UNKNOWN_ITEM_NAME):Cogject(TallyVector(size), name ) {
    override val stateValue = stateType.valueToVector(value)
    fun getValue(): Int {
        return (stateType as TallyVector).getValue(stateValue)
    }

    override fun copy(): Cogject {
        return IntCogject(value, size, name)
    }
}

open class RandomIntCogject(name: String):IntCogject(Random.nextInt(0, 10), 10, name)

open class SetCogject(private val items: Array<String>,private val set: Set<String>, name: String = UNKNOWN_ITEM_NAME) : Cogject(SetVector(items), name) {
    override val stateValue = FloatArray(items.size){ i -> if (set.contains(items[i])) 1F else 0F}
    fun getValue() : Set<String> {
        return HashSet<String>().apply{addAll((stateType as SetVector).getSet(stateValue).map{ it-> it as String})}
    }

    override fun copy(): Cogject {
        return SetCogject(items, set, name)
    }
}



abstract class ValueCogject(private val type: UnsignedNumVectorType,value: Any, name: String): Cogject(type, name) {
    abstract fun getValue(): Any
    override val stateValue = type.valueToVector(value)

}